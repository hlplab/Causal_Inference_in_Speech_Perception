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

 // an outgrowth of `newLabelingBlock`
// PROBABLY SHOULDN'T BE USED WITH needsResp
function newSubtitleBlock(params) {
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
        case 'practiceMode':
            this.practiceMode = params[p];
            break;
        case 'respKeys':
            this.respKeys = params[p];
            break;
        case 'categories':
            this.categories = params[p];
            break;
        case 'blockNum':
            this.blockNum = params[p];
            break;
        case 'subtitles':
            this.subtitles = params[p];
            break;
        case 'subtitlePosition':
            this.subtitlePosition = params[p];
            break;
        case 'catchMethod':
            this.catchMethod = params[p];
            break;
        case 'timeoutMS':
            this.timeoutMS = params[p];
            break;
        case 'replacementCatchText':
            this.replacementCatchText = params[p];
            break;
        case 'gkThreshold':
        	this.gkThreshold = params[p];
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
    if (typeof(catchMethod) === 'undefined') {
        catchMethod = "none";
    }
    if (typeof(practiceMode) === 'undefined') {
        practiceMode = false;
    }
    if (typeof(subtitlePosition) === 'undefined') {
        subtitlePosition = "during";
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


newSubtitleBlock.prototype = {
    reps: undefined,
    blockReps: 1,
    stims: [],
    n: 0,
    respKeys: undefined, //{71: 'B', 72: 'D'},
    categories: undefined, // ['B', 'D']
    ncorrect: 0,
    keyCapture: false,
    tResp: -1,
    tStart: -1,
    tStartLoad: -1, //time it takes to load
    LoadTime: -1,
    ITI: 1000,
    auds: [],
    namespace: '',
    respField: undefined,
    onEndedBlock: undefined,
    pbIncrement: undefined,
    blockRandomizationMethod: undefined,
    totalUniqueTrials: undefined,
    currentKeyEvent: "NORESP", // default key event (when no one responds), var stores keypress
    timeoutMS: 99999999999, // a bit hacky
    practiceMode: false,  //practice mode: restarts block if you get anything wrong
    shouldPause: false, // if you want the trial to pause, eg to scold them
    myTimer: undefined,
    pressedSpace: false,
    instructionTest: false,
    replacementCatchText: false,  // if text, will replace the instructions for catch trials
    catchMethod: 'none', //'none' (ignore catch trials), 'aud_keep' (no changes), 'red_text' (red text), 'aud_replace', (beeps instead of playing audio) 'aud_before' (plays beep before audio)
    subtitlePosition: 'during', // haven't really decided yet

    // This is for the the gatekeeper
    gkThreshold: -1,  // -1 means no d' threshold
    dprimeCounter: {  // This keeps track of d'
    	"hits": 0, "misses": 0,
    	"falseAlarms": 0, "correctRejects": 0
    },

    run: function() {
        var _self = this;
        _self.init();
       _self.next();
    },

    init: function(opts) {
        var _self = this;

        // initialize trial counter
        this.n = 0;
        _self.totalUniqueTrials = _self.stimuliObj.filenames.length;

        ////////////////////////////////////////////////////////////////////////////////
        // initialize response keys and response labels:
        // response keys can be provided to constructor, or default to global var respKeyMap
        if (typeof this.respKeys === 'undefined') {
            this.respKeys = respKeyMap;
        }

        // likewise response labels ('categories') can be provided to the constructor or
        // set from the global (if it exists), or default to being extracted from the values
        // of the response key mapping.
        if (typeof this.categories === 'undefined') {
            // populate the category names from the global vector if it exists, or extract from the resp keys
            if (typeof categories === 'undefined') {
                this.categories = [];
                for (k in this.respKeys) {
                    this.categories.push(this.respKeys[k]);
                }
            } else {
                this.categories = categories;
            }
        }

        if (!validateRespKeys(this.respKeys, this.categories)) {
            return false;
        }


        ////////////////////////////////////////////////////////////////////////////////
        // Randomize stimuli order.
        // default to "calibReps" reps property of this.stimuliObj for reps of each
        // stimulus.
        this.stims = [];
        for (var br = 0; br < this.blockReps; br++) {
            this.stims = this.stims.concat(pseudoRandomOrder(this.reps, this.totalUniqueTrials, this.blockRandomizationMethod));
        }
        console.log(this.stims);

        this.pbIncrement = 1.0 / this.stims.length;

        ////////////////////////////////////////////////////////////////////////////////
        // Bind handlers for this block:
        // create handler to capture and process keyboard input, and bind to document
        $(document).on('keyup.' + this.namespace, function(e) {_self.handleResp(e);});

        ////////////////////////////////////////////////////////////////////////////////
        // Initialize UI elements
        // set task instructions and response cues
        $("#taskInstructions").html('Press <span id="bKey" class="respKey">' +
                                    valToKey(this.respKeys, this.categories[0]) +
                                    '</span> for "' + this.categories[0] + '"<br />' +
                                    'Press <span id="dKey" class="respKey">' +
                                    valToKey(this.respKeys, this.categories[1]) + '</span> for "' + this.categories[1] + '"');
        this.instructionText =  $("#taskInstructions").html();
        $("#taskInstructions").hide();

        installPB("progressBar");
        resetPB("progressBar");
        $("#progressBar").show();
        if (this.catchMethod=="aud_replace" || this.catchMethod=="aud_before") {
            $beep = $('<audio>')
            $beep.attr("src", 'JSEXP/sounds/beep.wav');
        }
        // DEBUGGING: add button to force start of calibration block (skip preview)
        $('#buttons').append('<input type="button" onclick="calibrationBlock.next()" value="start calibration"></input>');
    },

    isCatchTrial: function() {
        var is_catch_trial = typeof(this.stimuliObj.isCatch) != "undefined" && this.stimuliObj.isCatch[this.stims[this.n]] === 'Y';
        return is_catch_trial && this.catchMethod != "none";
    },


    // Opens up to accept keystrokes
    openKeyCapture: function() {
        var _self = this;
        _self.keyCapture=true;
    },
    // Closes ability to accept keystrokes
    closeKeyCapture: function() {
        var _self = this;
        _self.keyCapture=false;
    },

    handleResp: function(e) {
    	_self = this;
        $('#testStatus').html('keyup() detected: keycode = ' + e.which + ' (' +
                              String.fromCharCode(e.which) + ')');
        if (this.keyCapture) {
            if (e.which === 32) {
                if (this.isCatchTrial()) {
                	_self.dprimeCounter["hits"] = _self.dprimeCounter["hits"] + 1;
                    $("#CatchFeedback").show();
                } else {
                	_self.dprimeCounter["falseAlarms"] = _self.dprimeCounter["falseAlarms"] + 1;
                    chastise_text = "Please only press the spacebar on a catch trial!";
                    $("#wrongAnsMessage").html(chastise_text);
                    $("#wrongAnsMessage").show();
                    this.shouldPause = true;
                }
            } else {
                chastise_text = "This is the <b>listening</b> task. Do not press anything but the spacebar, and only press that on the catch trials.";
                $("#wrongAnsMessage").html(chastise_text);
                $("#wrongAnsMessage").show();
                this.shouldPause = true;
            }
            this.tResp = Date.now();
            this.closeKeyCapture();
            this.currentKeyEvent = e;
        }
    },

    // handle end of trial
    end: function() {
        _self = this;
        this.closeKeyCapture();
        $("#subtitle").empty();
        $("#CatchFeedback").hide();

        // If you didn't respond to a catch trial
        if (_self.isCatchTrial() && _self.currentKeyEvent == "NORESP") {
        	_self.dprimeCounter["misses"] = _self.dprimeCounter["misses"] + 1;
            chastise_text = "That was a catch trial. Be sure to press the spacebar for these!";
            $("#wrongAnsMessage").html(chastise_text);
            $("#wrongAnsMessage").show();
            _self.shouldPause = true;
        }
        if (!_self.isCatchTrial() && _self.currentKeyEvent == "NORESP") {
        	_self.dprimeCounter["correctRejects"] = _self.dprimeCounter["correctRejects"] + 1;
        }

        // If you did something that requires pausing, pause for 3 seconds
        if (_self.shouldPause) {
            setTimeout(function(){
                _self.shouldPause = false;
                // If you're in practice mode, show a message and restart
                if (_self.practiceMode) {
                	try_again_text = "In a few seconds, we'll restart the practice session to give you more time to practice.<br />" +
                	"Make sure to pay attention and press spacebar only when you hear a beep or when the text says so.<br />"+
                	"In the main part of the experiment, you'll see a warning every time you miss one of these trials.<br />"+
                	"Whenever that happens, it's a reminder to pay more attention."
                    $("#wrongAnsMessage").html(try_again_text);
                    $("#wrongAnsMessage").show();
                    _self.n = 0;
                    resetPB("progressBar");
                    // Get rid of data you had written
                    $(_self.respField).val('');
                    setTimeout(function() {
                        _self.next();
                    }, 10000);
                } else {
                    // update progress bar
                    plusPB("progressBar", _self.pbIncrement);
                    // record response
                    _self.recordResp();
                }
            }, 3000);
        } else {
            // update progress bar
            plusPB("progressBar", _self.pbIncrement);
            // record response
            _self.recordResp();
        }
    },

    // start next trial
    next: function() {
        var _self = this;
        console.log(_self.dprimeCounter);
        var dpC = _self.dprimeCounter;
        var dprime = calculateDPrime(dpC["hits"], dpC["misses"], dpC["falseAlarms"], dpC["correctRejects"]);
        console.log(dprime);

        // some status information (hidden by default)
        $('#testStatus').append('<br />stims: ' + this.stims + ', n: ' + this.n);
        $('#testStatus').html('...wait');
        $("#wrongAnsMessage").hide();

        var isCatch = _self.isCatchTrial();
        if (_self.replacementCatchText && isCatch) {
            $("#taskInstructions").html(_self.replacementCatchText);
        } else {
            $("#taskInstructions").html(_self.instructionText);
        }

        // pause before next fixation cross appears
        console.log(_self.stims);

        var extension = ".wav";

        // If the filename already has the extension in it, don't put it in (ZACH EDIT)
        var current = _self.stimuliObj.prefix + _self.stimuliObj.filenames[_self.stims[_self.n]];
        if (_self.stimuliObj.filenames[_self.stims[_self.n]].indexOf(extension) == -1) {
          current = current + extension;
        }
        if (isCatch && _self.catchMethod == "aud_before") {
            $beep[0].play();
        }

        if (isCatch || this.subtitlePosition === "during" || this.subtitlePosition === "absent") {
            // Immediately plays
            setTimeout(function() {
                // Boilerplate -------------------------------------------------------
                $("#CatchFeedback").hide();
                $originalAudio = $('<audio>');
                _self.tStartLoad = Date.now();
                if (isCatch && _self.catchMethod == "aud_replace") {
                    $originalAudio.attr("src", 'JSEXP/sounds/beep.wav');
                } else {
                    $originalAudio.attr("src", current);
                }
//                 keyPress = true;  // don't know what this is
                var aud_dur;
                $originalAudio.on("canplay", function() {
                    _self.LoadTime = Date.now() - _self.tStartLoad;
                    aud_dur = $originalAudio[0].duration * 1000;
                    var total_trial_time = aud_dur*2 // the total amount of time until the next trial

                    $originalAudio[0].play();
                    // End of boilerplate ------------------------------------------------

                    $originalAudio.on('play', function() {
                        $("#subtitle").text(_self.stimuliObj.subtitles[_self.stims[_self.n]]);
                        $("#subtitle").show();
                        if (isCatch && _self.catchMethod == "red_text") {
                            var og_text = $("#subtitle").text();
                            $("#subtitle").html('<span style="color:red">'+og_text+'</span>');
                        }
                        _self.tStart = Date.now();
                        _self.openKeyCapture();

                        $originalAudio.on('ended', function() {
                            $("#subtitle").empty();
                            $("#subtitle").hide();

                            setTimeout(function() {
                                _self.end();
                            }, aud_dur);
                        });
                    });
                });
            }, _self.ITI);  // waint until ITI done before next trial
        } else if (this.subtitlePosition === "delayed") {
            // Immediately plays
            setTimeout(function() {
                // Boilerplate -------------------------------------------------------
                $("#CatchFeedback").hide();
                $originalAudio = $('<audio>');
                _self.tStartLoad = Date.now();
                if (isCatch && _self.catchMethod == "aud_replace") {
                    $originalAudio.attr("src", 'JSEXP/sounds/beep.wav');
                } else {
                    $originalAudio.attr("src", current);
                }

                var aud_dur;
                $originalAudio.on("canplay", function() {
                    _self.LoadTime = Date.now() - _self.tStartLoad;
                    aud_dur = $originalAudio[0].duration * 1000;
                    var total_trial_time = aud_dur*2 // the total amount of time until the next trial

                    $originalAudio[0].play();
                    // End of boilerplate ------------------------------------------------

                    $originalAudio.on('play', function() {
                    	_self.tStart = Date.now();
                        _self.openKeyCapture();

                    	setTimeout(function() {
                    		$("#subtitle").text(_self.stimuliObj.subtitles[_self.stims[_self.n]]);
							$("#subtitle").show();
							if (isCatch && _self.catchMethod == "red_text") {
								var og_text = $("#subtitle").text();
								$("#subtitle").html('<span style="color:red">'+og_text+'</span>');
							}

							setTimeout(function() {
								$("#subtitle").empty();
								$("#subtitle").hide();
								_self.end();

							}, aud_dur);
                    	}, aud_dur);
                    });
                });
            }, _self.ITI);  // waint until ITI done before next trial
        }


        console.log("Start time:");
        console.log(_self.tStart);
        console.log(Date.now()-_self.tStart);
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
       // $(this.auds).unbind('.' + this.namespace).height(0);
       // $(document).unbind('.' + this.namespace);

       var dpC = this.dprimeCounter;
       var dprime = calculateDPrime(dpC["hits"], dpC["misses"], dpC["falseAlarms"], dpC["correctRejects"]);


       if (this.gkThreshold > 0 && dprime < this.gkThreshold) {
           // defined in exp3.js
           failed_dprime = true;
           console.log("DPRIME = "+dprime+", threshold = "+this.gkThreshold);
       }


        $(document).off();
        if (typeof(this.onEndedBlock) === 'function') {
            this.onEndedBlock();
        } else {
            if (console) console.log('WARNING: End of block reached but no callback found');
        }
    },

    // return info on current state in string form
    info: function() {
        // alert('stims: ' + this.stims + ', n: ' + this.n);
        var _self = this;
        return [_self.namespace, // what part of experiment
                _self.stims[_self.n], // the number in the block
                _self.stimuliObj.filenames[_self.stims[_self.n]], // the file name
                _self.stimuliObj.subtitles[_self.stims[_self.n]]].join(); //the subtitle
    },

    // method to handle response. takes event object as input
    recordResp: function() {
        var e = this.currentKeyEvent;
        this.currentKeyEvent = "NORESP";
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
        if (typeof(_self.stimuliObj.isCatch) != "undefined") {
            var iscatch = _self.stimuliObj.isCatch[_self.stims[_self.n]];
        } else {
            var iscatch;
        }
        var responsefailure = e == "NORESP";
        if (responsefailure) {
            this.tResp = -1;
        }

        var resp = [this.info(), e.which,
                    "", //empty string, since String.fromCharCode(e.which) might have been causing issues
                    iscatch, this.catchMethod, responsefailure,
                    this.tStart, this.tResp, this.tResp-this.tStart,
                    workerid, condition, list_num, url_str, this.LoadTime].join();

        // write info to form field
        if (this.n < this.stims.length) {
            $(this.respField).val($(this.respField).val() + resp + RESP_DELIM);
        }
        // if more trials remain, trigger next trial
        if (++this.n < this.stims.length) {
            this.next();
        } else {
            this.endBlock();
        }
        return;
    },
}

// link up via __super__ to superclass, etc.
extend(TestBlock, newSubtitleBlock);



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


//// D PRIME STUFFF ........ //////////////////////

/*  The following JavaScript functions for calculating normal and
    chi-square probabilities and critical values were adapted by
    John Walker from C implementations
    written by Gary Perlman of Wang Institute, Tyngsboro, MA
    01879.  Both the original C code and this JavaScript edition
    are in the public domain.  */

/*  POZ  --  probability of normal z value

    Adapted from a polynomial approximation in:
            Ibbetson D, Algorithm 209
            Collected Algorithms of the CACM 1963 p. 616
    Note:
            This routine has six digit accuracy, so it is only useful for absolute
            z values <= 6.  For z values > to 6.0, poz() returns 0.0.
*/
var Z_MAX = 6;
function poz(z) {

    var y, x, w;

    if (z == 0.0) {
        x = 0.0;
    } else {
        y = 0.5 * Math.abs(z);
        if (y > (Z_MAX * 0.5)) {
            x = 1.0;
        } else if (y < 1.0) {
            w = y * y;
            x = ((((((((0.000124818987 * w
                     - 0.001075204047) * w + 0.005198775019) * w
                     - 0.019198292004) * w + 0.059054035642) * w
                     - 0.151968751364) * w + 0.319152932694) * w
                     - 0.531923007300) * w + 0.797884560593) * y * 2.0;
        } else {
            y -= 2.0;
            x = (((((((((((((-0.000045255659 * y
                           + 0.000152529290) * y - 0.000019538132) * y
                           - 0.000676904986) * y + 0.001390604284) * y
                           - 0.000794620820) * y - 0.002034254874) * y
                           + 0.006549791214) * y - 0.010557625006) * y
                           + 0.011630447319) * y - 0.009279453341) * y
                           + 0.005353579108) * y - 0.002141268741) * y
                           + 0.000535310849) * y + 0.999936657524;
        }
    }
    return z > 0.0 ? ((x + 1.0) * 0.5) : ((1.0 - x) * 0.5);
}


/*  qnorm  --  Compute critical normal z value to
               produce given p.  We just do a bisection
               search for a value within CHI_EPSILON,
               relying on the monotonicity of pochisq().  */

function qnorm(p) {
    var Z_EPSILON = 0.000001;     /* Accuracy of z approximation */
    var minz = -Z_MAX;
    var maxz = Z_MAX;
    var zval = 0.0;
    var pval;
    if( p < 0.0 ) p = 0.0;
    if( p > 1.0 ) p = 1.0;

    while ((maxz - minz) > Z_EPSILON) {
        pval = poz(zval);
        if (pval > p) {
            maxz = zval;
        } else {
            minz = zval;
        }
        zval = (maxz + minz) * 0.5;
    }
    return(zval);
}

// Calculate d'
function calculateDPrime(n_hit, n_miss, n_fa, n_cr) {
  // Ratios
  var hit_rate = n_hit / (n_hit + n_miss);
  var fa_rate  = n_fa /  (n_fa + n_cr);

  // Adjusted ratios
  var hit_rate_adjusted = (n_hit+0.5) / ((n_hit+0.5) + n_miss + 1);
  var fa_rate_adjusted  = (n_fa+0.5)  / ((n_fa+0.5) + n_cr + 1);

  return (qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted));
}
