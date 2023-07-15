/*
 * Author: Dave F. Kleinschmidt
 *
 *    Copyright 2012 Dave Kleinschmidt and
 *        the University of Rochester BCS Department
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU Lesser General Public License version 2.1 as
 *    published by the Free Software Foundation.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public License
 *    along with this program.
 *    If not, see <http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html>.
 * 
 */
 
 /*
  * THIS BLOCK IS JUST A COMPLETE HACK!
  *
  */
 
 
function surveyBlock(params) {
    // process parameters
    var stimuliObj, instructions, namespace, css_stim_class;
    for (p in params) {
        switch(p) {
        case 'stimuli':
            stimuliObj = params[p];
            break;
        case 'instructions':
            instructions = params[p];
            break;
        case 'namespace':
            namespace = params[p];
            break;
        case 'reps':
            this.reps = params[p];
            break;
        case 'blockReps':
            this.blockReps = params[p];
            break;
        case 'blockRandomizationMethod':
            this.blockRandomizationMethod = params[p];
            break;
        case 'ITI':
            this.ITI = params[p];
            break;
        case 'blockNum':
            this.blockNum = params[p];
            break;
        case 'timeoutMS':
            this.timeoutMS = params[p];
            break;
        case 'bad_answer_dictionary':
            this.bad_answer_dictionary = params[p];
            break;
        case 'question_names':
            this.question_names = params[p];
            break;
        case 'question_list':
            this.question_list = params[p];
            break;
        default:
            break;
        }
    }

    // set namespace for this block (prefix for events/form fields/etc.) and
    // the class that the stimuli are assigned
    if (typeof(namespace) === 'undefined') {
        var namespace = '';
        var css_stim_class = 'stim';
    } else {
        var css_stim_class = namespace + 'stim';
        this.namespace = namespace;
    }
    // install stimuli
    if (isArray(stimuliObj)) {
        // concatenate into one mega-object, and set for this block
        // NOTE / TODO: I'm uncertain if this part of the loop works... when would you need it?
        this.stimuliObj = concatenate_stimuli_and_install(stimuliObj, css_stim_class);
        this.auds = this.stimuliObj.installed;
    } else {
        // set stimuli object for this block
        this.stimuliObj = stimuliObj;
        $('#continue').show();
    }

    // create responses form element and append to form
    this.respField = $('<textArea id="' + namespace + 'Resp" ' +
                       'name="' + namespace + 'Resp" ></textArea>').appendTo('#mturk_form');
    $('#mturk_form').append('<br />');

}


surveyBlock.prototype = {
    reps: undefined,
    blockReps: 1,
    stims: [],
    n: 0,
    ncorrect: 0,
    tResp: -1,
    tStart: -1,
    tStartLoad: -1, //time it takes to load
    ITI: 0,
    auds: [],
    namespace: '',
    respField: undefined,
    onEndedBlock: undefined,
    pbIncrement: undefined,
    blockRandomizationMethod: undefined,
    timeoutMS: 99999999999, // a bit hacky
    myTimer: undefined,
    isBlocked: false,
    bad_answer_dictionary: {
        "audio_stall": ["yes", "some", "NORESP"],
        "audio_type":  ["computer speakers", "external", "NORESP"],
        "bilingual":   ["yes", "NORESP"],
        "location":    ["no", "NORESP"],
    },
    question_names: ['audio_stall', 'bilingual', 'audio_type', 'location'],
    question_list: [
        '<p name="audio_stall">Did any of the audio clips jump, stall, or skip during the experiment?</p>' +
        '<input id="sb_audio_stall_yes" name="survey_block_q" type="radio" value="yes"/><label for="sb_audio_stall_yes">Yes, very frequently.</label><br/>' +
        '<input id="sb_audio_stall_some" name="survey_block_q" type="radio" value="some"/><label for="sb_audio_stall_some">Yes, a handful of times.</label><br/>' +
        '<input id="sb_audio_stall_few" name="survey_block_q" type="radio" value="few"/><label for="sb_audio_stall_few">Yes, once or twice.</label><br/>' +
        '<input id="sb_audio_stall_no" name="survey_block_q" type="radio" value="no"/><label for="sb_audio_stall_no">No, they all played smoothly.</label><br/>',
        
        '<p name="bilingual">Can you understand any language other than English?</p>' +
        '<input id="sb_ml_yes" name="survey_block_q" type="radio" value="yes"/><label for="sb_ml_yes">Yes.</label><br/>' +
        '<input id="sb_ml_slight" name="survey_block_q" type="radio" value="slight"/><label for="sb_ml_slight">At <em>most</em> I would struggle if listening to a normal conversation.</label><br/>' +
        '<input id="sb_ml_no" name="survey_block_q" type="radio" value="no"/><label for="sb_ml_no">No.</label><br/>',
        
        '<p>What kind of audio equipment did you use for the experiment? Be honest. Your response will not influence your payment.</p>' +
        '<input id="sb_audio_type_in-ear" name="survey_block_q" type="radio" value="in-ear"/><label for="sb_audio_type_in-ear">In-ear headphones</label><br/>' +
        '<input id="sb_audio_type_over-ear" name="survey_block_q" type="radio" value="over-ear"/><label for="sb_audio_type_over-ear">Over-the-ear headphones</label><br/>' +
        '<input id="sb_audio_type_computer speakers" name="survey_block_q" type="radio" value="computer speakers"/><label for="sb_audio_type_computer speakers">Laptop Speakers</label><br/>' +
        '<input id="sb_audio_type_external" name="survey_block_q" type="radio" value="external"/><label for="sb_audio_type_external">External Speakers</label><br/>',
        
        '<p>Did you take this experiment in a quiet location? Be honest. Your response will not influence your payment.</p>' +     
        '<input id="sb_location_yes" name="survey_block_q" type="radio" value="yes"/><label for="sb_location_yes">Yes.</label><br/>' + 
        '<input id="sb_location_no"  name="survey_block_q" type="radio" value="no" /><label for="sb_location_no">No.</label><br/>'
    ],


    run: function() {
        var _self = this;
        _self.init();
       _self.next();
    },

    init: function(opts) {
        var _self = this;

        // initialize trial counter
        this.n = 0;
        this.pbIncrement = 1.0 / _self.question_list.length;
        console.log(this.ITI);
        ////////////////////////////////////
        /// Header and footer
        var q_head1 = '<div class="blocksurvey" id="XXXX" style="width:600px;margin-left:auto;margin-right:auto;">' + 
                     '<div class="survey_question_section" id="';
        var q_head2 = '" style="display:visible">';
        var q_foot = '<input class="moveOnSurveyBlock" id="SurveyBlockContinue" name="Continue" type="button" value="Continue" style="display:block;"/>' +
                     '</div></div>';
                     
        // add them on, along with the names
        for (var i = 0, length = _self.question_list.length; i < length; i++) { 
            this.question_list[i] = q_head1 + this.question_names[i] + q_head2 + 
                                    this.question_list[i] + q_foot
        }
        $("#subtitle").html('');
        $("#subtitle").empty();
        $("#subtitle").html(this.question_list[this.n]);
        $("#subtitle").hide();
        ///////////////////////////////////////////        
        
        installPB("progressBar");
        resetPB("progressBar");
        $("#progressBar").show();
    },

    waitForResp: function() {
        var _self = this;
        $("#SurveyBlockContinue").on('click', function(e){console.log("AAAAAA"); _self.handleResp();});
        _self.myTimer = setTimeout(function() {        
            _self.tResp = -1;
            _self.end(["NORESP", document.getElementsByClassName('survey_question_section')[0]]);
            }, _self.timeoutMS);
    },

    handleResp: function() {
        var _self = this;
        if (_self.isBlocked==false) {
            var radios = document.getElementsByName('survey_block_q');
            for (var i = 0, length = radios.length; i < length; i++) {
              if (radios[i].checked) {
                var this_resp = [radios[i].value, document.getElementsByClassName('survey_question_section')[0]];
                
                this.tResp = Date.now();
                clearTimeout(this.myTimer);
                
                this.end(this_resp);
                // only one radio can be logically checked, don't check the rest
                break;
              }
            }
        }
    },

    // start next trial
    next: function() {
        var _self = this;
        console.log(_self.ITI);
        $("#subtitle").hide();
        // wait ITI until playing next stimuli (asynchronously with fixation display)
        setTimeout(function() {
             _self.tStartLoad = Date.now();
             $("#subtitle").html(_self.question_list[_self.n]);
             $("#subtitle").show();
            _self.isBlocked = false;
             _self.tStart = Date.now();
             _self.waitForResp();
        }, _self.ITI);
    },

    // handle end of trial (called by key press handler)
    end: function(e) {
        _self.isBlocked = true;
        // update progress bar
        plusPB("progressBar", this.pbIncrement);
        // record response
        this.recordResp(e);
    },

    endBlock: function() {
        // trigger endCalibrationBlock event
        $('#fixation').hide();
        $("#taskInstructions").hide();
        $("#progressBar").hide();
        $("#instructionsLower").hide();
        $("#subtitle").hide();
        $("#wrongAnsMessage").hide();
        $("#CatchFeedback").hide();
        console.log($(this.respField).val());
        $(document).off();
        if (typeof(this.onEndedBlock) === 'function') {
            this.onEndedBlock();
        } else {
            if (console) console.log('WARNING: End of block reached but no callback found');
        }
    },

    // return info on current state in string form
    info: function() {
        var _self = this;
        return [_self.namespace, // what part of experiment
                _self.n, _self.question_names[_self.n]].join(); //the subtitle
    },

    // method to handle response. takes event object as input
    recordResp: function(e) {
        // format trial information
        this.urlparams = gupo();
        var workerid = this.urlparams['workerId'];
        var condition = this.urlparams['condition']; //ACCENTED, UNACCENTED, NOISE
        var list_num = this.urlparams['list'];    //{1-4}[_rev]
        var url_str = [];
        for(var p in this.urlparams)
            if (this.urlparams.hasOwnProperty(p)) {
                url_str.push(encodeURIComponent(p) + "=" + encodeURIComponent(this.urlparams[p]));
            }
        url_str = url_str.join("&");
        
        var _self = this;        
        var resp = [this.info(), e[1], e[0],
                    this.tStart, this.tResp, this.tResp-this.tStart,
                    workerid, condition, list_num, url_str].join();
        
        // Does the response contain a bad answer?
        var cur_q = _self.question_names[_self.n];
        var cur_bad = _self.bad_answer_dictionary[cur_q];
        // Set this previously defined (in the exp.js file) variable
        if (cur_bad.indexOf(e[0]) >= 0) {
            bad_survey_response_bool = true;
            alert("YOU MESSED UP!");
            alert(cur_q); alert(cur_bad); alert(cur_bad.indexOf(e[0])); alert(e[0]);
        }

        // write info to form field
        if (this.n < this.question_list.length) {
            $(this.respField).val($(this.respField).val() + resp + RESP_DELIM);
        }
        // if more trials remain, trigger next trial
        if (++this.n < this.question_list.length) {
            this.next();
        } else {
            this.endBlock();
        }
        return;
    },
}

// link up via __super__ to superclass, etc.
extend(TestBlock, surveyBlock);



//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/array/shuffle [rev. #1]
//  shuffle the input array
// DEPRECATED: use version in utilities.js
var shuffle = function(v){
    if (console) console.log('WARNING: labelingblock.js:pseduoRandomOrder is deprecated.  use utilities.js:pseudoRandomOrder instead');

    for(var j, x, i = v.length; i; j = parseInt(Math.random() * i), x = v[--i], v[i] = v[j], v[j] = x);
    return v;
};

// Some vector math helper functions (get max, min, range, and sum of a numeric Array)
Array.max = function( array ){
    return Math.max.apply( Math, array );
};
Array.min = function( array ){
    return Math.min.apply( Math, array );
};
Array.range = function(array) {
    return Array.max(array) - Array.min(array);
};
Array.prototype.sum = function() {
    var s=0;
    for (var i=0; i<this.length; i++) {
        s += this[i];
    };
    return(s)
};

// reverse map lookup (get key given value)
function valToKey(obj, v) {
    var keys = [];
    for (k in obj) {
        if (obj[k]==v) {
            keys.push(k);
        }
    }
    return(keys);
}

// Function to detect if object is an array, from http://stackoverflow.com/a/1058753
function isArray(obj) {
    return Object.prototype.toString.call(obj) === '[object Array]';
}

// classical-esque class inheritance: sets prototype of prototype to superclass prototype
function extend(child, supertype)
{
    child.prototype.__proto__ = supertype.prototype;
    child.prototype.__super__ = supertype.prototype;
}

function validateRespKeys(respKeys, categories) {
    for (k in respKeys) {
        if (! categories.has(respKeys[k])) {
            if (console) console.log('ERROR: response label {0} not found in specified categories {1}'.format(respKeys[k], categories));
            return false;
        }
    }
    return true;
}

