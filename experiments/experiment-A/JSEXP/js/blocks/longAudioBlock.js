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
var keyPress = false;
function LongAudioBlock(params) {
    // process parameters
    var stimuliObj, namespace, css_stim_class;
    for (p in params) {
        switch(p) {
        case 'stimuli':
            stimuliObj = params[p];
            break;
        case 'instructions':
            this.instructions = params[p];
            break;
        case 'mediaType':
            this.mediaType= params[p];
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
        case 'subtitlePosition':
            this.subtitlePosition = params[p];
            break;
        case 'isPractice':
            this.isPractice = params[p];
            break;
        case 'catchTrialText':
            this.catchTrialText = params[p];
            break;
        case 'blockNum':
            this.blockNum = params[p];
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
    if (typeof(isPractice) === 'undefined') {
        isPractice = false;
    }

    if (isArray(stimuliObj)) {
        // concatenate into one mega-object, and set for this block
        console.log("YO you should figure out this loop here");
        this.stimuliObj = concatenate_stimuli_and_install(stimuliObj, css_stim_class);
        this.auds = this.stimuliObj.installed;
    } else {
        // set stimuli object for this block
        this.stimuliObj = stimuliObj;
        console.log(stimuliObj);
        $('#continue').show();
    }

    // create responses form element and append to form
    this.respField = $('<textArea id="' + namespace + 'Resp" ' +
                       'name="' + namespace + 'Resp" ></textArea>').appendTo('#mturk_form');
    $('#mturk_form').append('<br />');

}

LongAudioBlock.prototype = {
    reps: undefined,
    blockReps: 1,
    stims: [], // the indices WITH randomiziation
    n: 0,
    ncorrect: 0,
    tResp: -1,
    tStart: -1,
    tStartLoad: -1, //time it takes to load
    LoadTime: -1,
    ITI: 1000,
    namespace: '',
    respField: undefined,
    onEndedBlock: undefined,
    pbIncrement: undefined,
    blockRandomizationMethod: undefined,
    totalUniqueTrials: undefined,
    instructions: undefined,
    mediaType: "audio",
    pressedSpace: false,
    isPractice: false,
    catchTrialText: "Please press the space bar now",
    cancellableTimeout: undefined,
    blockNum: undefined,

    run: function() {
        var _self = this;
        _self.init();
       _self.next();
    },

    init: function(opts) {
        console.log("In init");
        var _self = this;

        _self.totalUniqueTrials = _self.stimuliObj.filenames.length;

        console.log("trials: " + _self.totalUniqueTrials);
        // initialize trial counter
        this.n = 0;

        ////////////////////////////////////////////////////////////////////////////////
        // Randomize stimuli order.

        this.stims = [];
        for (var br = 0; br < this.blockReps; br++) {
            this.stims = this.stims.concat(pseudoRandomOrder(this.reps, this.totalUniqueTrials, this.blockRandomizationMethod));
        }
        this.pbIncrement = 1.0 / this.stims.length;
        //Move to next trial on click

        $(document).on('keydown.' + this.namespace, function(e) {_self.handleRespDown(e);});
        $(document).on('keyup.' + this.namespace, function(e) {_self.handleResp(e);});

        $("#instructionsLower").html(_self.instructions);

        // install, initialize, and show a progress bar (progressBar.js)
        installPB("progressBar");
        resetPB("progressBar");
        $("#progressBar").show();
        // DEBUGGING: add button to force start of calibration block (skip preview)
    },

    isNCatchTrial: function(n) {
      var _self = this;
      if (_self.stimuliObj.subends[_self.stims[_self.n]] != 0) {
        return true;
      } else {
        return false;
      }
    },

    handleResp: function(e) {
        //Press space for next trial
        if (keyPress === true) {
            if (e.keyCode === 0 || e.keyCode === 32) {
                this.pressedSpace = true;
                this.tResp = Date.now();
                $('#CatchFeedback').show();
            }
        }
    },

    handleRespDown: function(e) {
        //Press space for next trial
        if (keyPress === true) {
            if (e.keyCode === 0 || e.keyCode === 32) {
                e.preventDefault();
                return false;
            }
        }
    },

    // start next trial
    next: function() {
        var _self = this;
        this.pressedSpace = false;
        // some status information (hidden by default)
        $('#testStatus').append('<br />stims: ' + this.stims + ', n: ' + this.n);
        $('#testStatus').html('...wait');

        $("#instructionsLower").show();
        $("#wrongAnsMessage").hide();

        var extension = "";
        if (_self.mediaType === "audio") {
            extension = ".wav";
        }
        if (_self.mediaType === "video") {
            extension = ".webm";
        }
       var originalFile = _self.stimuliObj.prefix + _self.stimuliObj.filenames[_self.stims[_self.n]];
        if (_self.stimuliObj.filenames[_self.stims[_self.n]].indexOf(extension) == -1) {
          originalFile = originalFile + extension;
        }
        // keyPress = true;
        // If you want individual subtitles you can do that, but I'm just changing the instruction to a single value
        // $("#subtitle").text(_self.stimuliObj.subtitles[_self.stims[_self.n]]);
        $("#subtitle").text(_self.catchTrialText);


        $('#fixation').show();

        // Setting up how when to display text (used currently  )
        subtitleDisplayStart = _self.stimuliObj.substarts[_self.stims[_self.n]];
        subtitleDisplayEnd = _self.stimuliObj.subends[_self.stims[_self.n]];

        // This is where the magic happens, baby
        setTimeout(function() {
            $originalAudio = $('<audio>');
            _self.tStartLoad = Date.now();
            $originalAudio.attr("src", originalFile);
            $originalAudio.attr("preload", "auto");
            $originalAudio.on("canplay", function() {
                audLength = $originalAudio[0].duration * 1000;
                console.log(audLength);
                if (audLength < subtitleDisplayEnd) {
                    console.log("subtitle end time greater than total clip time");
                    console.log(subtitleDisplayEnd);
                }
                $originalAudio[0].play();
                $originalAudio.on('playing', function() {
                      _self.LoadTime = Date.now() - _self.tStartLoad;
                //    $('#catchTrialInstruction').hide();
                //    $('#fixation').hide();
                //    $("#subtitle").show();
                });
                // if true, show subtitle at the specified time and let responses happen then
                if (_self.isNCatchTrial(_self.n)) {
                    // Set when the subtitle appears and you can respond
                    setTimeout(function() {
                        keyPress = true;
                        $("#subtitle").show();
                        _self.tStart = Date.now();
                        }, subtitleDisplayStart);
                    // Set when the subtitle disappears and you can't
                    // However, to make it cancellable (so it doesn't interfere later with anything) we assign it a variable
                    _self.cancellableTimeout = setTimeout(function() {
                          keyPress = false;
                          $("#subtitle").hide();
                          $('#CatchFeedback').hide();
                          }, subtitleDisplayEnd);
                }
            });
            $originalAudio.on('ended', function() {
                keyPress = false;
                $("#subtitle").hide();
                $('#CatchFeedback').hide();
                // Here we cancel the timer so it doesn't accidentally hide anything later
                clearTimeout(_self.cancellableTimeout);
                _self.recordResp();
            });
        }, _self.ITI);
    },

    // handle end of trial (called by key press handler)
    end: function() {
        // update progress bar
        plusPB("progressBar", this.pbIncrement);

        //unbind things
        keyPress = false;
        $("#subtitle").empty();
        $("#subtitle").hide();

        // Reset tResp and tStart values (using "tStart != -1" to check if catchtrial")
        this.tResp = -1;
        this.tStart = -1;

        // if more trials remain, trigger next trial
        if (++this.n < this.stims.length) {
            this.next();
        } else {
            this.endBlock();
        }
    },

    endBlock: function() {
        // trigger endCalibrationBlock event
        $('#fixation').hide();
        $("#taskInstructions").hide();
        $("#progressBar").hide();
       // $(this.auds).unbind('.' + this.namespace).height(0);
       // $(document).unbind('.' + this.namespace);
        console.log("end block value")
        console.log($(this.respField).val())
        $("#instructionsLower").hide();
        $("#subtitle").hide();
        $("#CatchFeedback").hide();
        $(document).off();
        if (typeof(this.onEndedBlock) === 'function') {
            this.onEndedBlock();
        } else {
            if (console) console.log('WARNING: End of block reached but no callback found');
        }
    },

    // method to handle response. takes event object as input
    recordResp: function() {
        // format trial information
        _self = this;
        this.urlparams = gupo();
        var workerid = this.urlparams['workerId'];
        var condition = this.urlparams['condition'];
        var list = this.urlparams['list'];
        var resp = [this.namespace,  //e.g. 'practice' 'main' etc
                    this.n, // the original trial number in block
                    this.blockNum, // the block number
                    //this.stims[this.n], // the trial number it was presented in
                    this.stimuliObj.filenames[_self.stims[_self.n]], // the file name
                    this.pressedSpace, // whether they responded
                    this.tStart, // when they could first respond
                    this.tResp, // when they responsed
                    this.LoadTime, // load time
                    condition,  // condition
                    list, // list id
                    workerid].join('|');

        // write info to form field
        //$('#calibrationResp').val($('#calibrationResp').val() + resp + RESP_DELIM);
        if (this.n < this.stims.length) {
            console.log("Writing resp:" + resp);
            $(this.respField).val($(this.respField).val() + resp + RESP_DELIM);
        }
        _self.end();
    },

};


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
    }
    return(s);
};

// reverse map lookup (get key given value)
function valToKey(obj, v) {
    var keys = [];
    for (var k in obj) {
        if (obj[k]==v) {
            keys.push(k);
        }
    }
    return(keys);
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
