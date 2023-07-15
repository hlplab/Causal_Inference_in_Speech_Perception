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
var lastplayed = "";
if (typeof String.prototype.startsWith != 'function') {
  // see below for better implementation!
  String.prototype.startsWith = function (str){
    return this.indexOf(str) == 0;
  };
}

function IdentificationBlock(params) {
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
        case 'respKeys':
            this.respKeys = params[p];
            break;
        case 'catchEndsTrial':
            this.catchEndsTrial = params[p];
            break;
        case 'catchKeyCode':
            this.catchKeyCode = params[p];
            break;
        case 'catchEventDescription':
            this.catchEventDescription = params[p];
            break;
        case 'catchTrialInstruction':
            this.catchTrialInstruction = params[p];
            break;
        case 'catchTrialFeedbackTrue':
            this.catchTrialFeedbackTrue = params[p];
            break;
        case 'provideFeedback':
            this.provideFeedback = params[p];
            break;
        case 'enforcePerfection':
            this.enforcePerfection = params[p];
            break;
        case 'categories':
            this.categories = params[p];
            break;
        default:
            throwWarning('Unknown parameter passed to identificationBlock: ' + p);
            break;
        }
    }

    // set namespace for this block (prefix for events/form fields/etc.) and
    // the class that the stimuli are assigned
    if (typeof(namespace) === 'undefined') {
        var namespace = 'identification';
        var css_stim_class = 'stim';
    } else {
        var css_stim_class = namespace + 'stim';
        this.namespace = namespace;
    }

    // install stimuli
    if (typeof(stimuliObj) === 'undefined') {
        throwError("Unrecognized stimulus object.")
    }

    if (isArray(stimuliObj)) {
        // concatenate into one mega-object, and set for this block
        // NOTE / TODO: I'm uncertain if this part of the loop works... when would you need it?
        this.stimuliObj = concatenate_stimuli_and_install(stimuliObj, css_stim_class);
        this.auds = this.stimuliObj.installed;
    } else {
        // set stimuli object for this block
        this.stimuliObj = stimuliObj;
        stimuliObj.get_and_load_stims(css_stim_class); // this is asynchronous
        $('#continue').show();
    }

    // check whether everthing that enforcePerfection and/or provideFeedback require is available
    if (this.enforcePerfection && !this.provideFeedback) {
        throwError("cannot enforcePerfection if provideFeedback is false.");
    } else if (this.provideFeedback && typeof(this.stimuliObj.mappingStimulusToCorrectResponse) === 'undefined') {
        throwError("cannot provide feedback if stimulus list does not contain a mapping from stimuli to correct identification responses.");
    }

    // create responses form element and append to form, named by the block's name + "Resp"
    this.respField = $('<textArea id="' + namespace + 'Resp" ' +
                       'name="' + namespace + 'Resp" ></textArea>').appendTo('#mturk_form');
    $('#mturk_form').append('<br />');

}


IdentificationBlock.prototype = {
    reps: undefined,
    blockReps: 1,
    stims: [],
    n: 0,
    respKeys: undefined, // {71: 'B', 72: 'D'},
    provideFeedback: false,
    enforcePerfection: false,
    categories: undefined, // ['B', 'D']
    ncorrect: 0,
    keyCapture: false,
    tResp: -1,
    tStart: -1,
    ITI: 1000,
    auds: [],
    namespace: '',
    catchEndsTrial: true,
    catchKeyCode: 66, // "B"
    catchKeyText: undefined, // default defined below based on catchKeyCode
    catchEventDescription: 'a white dot',
    catchTrialInstruction: undefined, // default defined below based on catchKeyCode & catchEventDescription
    catchTrialFeedbackTrue: undefined, // default defined below based on catchKeyCode & catchEventDescription
    isCatchTrial: undefined,
    catchAns: false,
    respField: undefined,
    onEndedBlock: undefined,
    pbIncrement: undefined,
    blockRandomizationMethod: undefined,

    getTotalReps: function() {
        var reps;
        var blockReps = 1;
        if (typeof this.reps === 'undefined')  {
            reps = this.stimuliObj.calibReps;
        } else {
            reps = this.reps;
        }

        if (typeof this.blockReps !== 'undefined') {
            blockReps = this.blockReps;
        }

        if (reps.sum) {
            return(reps.sum());
        } else {
            return(reps * blockReps * this.stimuliObj.continuum.length);
        }
    },

    run: function() {
        var _self = this;
        this.init();
        _self.next();
    },

    init: function(opts) {
        throwMessage("Initiating block " + this.namespace);
        var _self = this;
        // takes the stimuli that were loaded asynchronology and matches them with the proper block.
        // Note that each block you installed should have a different namespace so this filters properly.
        var temp = _self.stimuliObj.get_installed();
        var temp2 = []
        for (var i=0; i<temp.length; i++) {
            if (temp[i].class.startsWith(_self.namespace + "stim")) {
                temp2.push(temp[i]);
            }
        }

        _self.auds = temp2;
        _self.stims = temp2;

        var softInit;
        // parse optional arguments object
        for (op in opts) {
            switch (op) {
            case 'softInit':
                softInit = true;
                break;
            default:
                throwWarning('Unrecognized Block init option: ' +
                                         op + ' (' + opts[op] + ')');
            }
        }

        // initialize trial counter
        this.n = 0;

        ////////////////////////////////////////////////////////////////////////////////
        // initialize response keys and response labels:
        // response keys can be provided to constructor, or default to global var respKeyMap
        if (typeof this.respKeys === 'undefined') {
            this.respKeys = respKeyMap;
        }


        // When catchKey is space set catchKeyText to "SPACE" else to catchKey
        if (this.catchKeyCode == 32) {
          this.catchKeyText = "SPACE";
        } else {
          this.catchKeyText = String.fromCharCode(this.catchKeyCode);
        }

        if (typeof(this.catchTrialInstruction) === 'undefined') {
          // set to '' to switch off this option (not yet implemented)
          this.catchTrialInstruction = 'Press "' + this.catchKeyText + '" when you see ' + this.catchEventDescription + '.';
        }

        if (typeof(this.catchTrialFeedbackTrue) === 'undefined') {
          // set to '' to switch off this option (not yet implemented)
          this.catchTrialFeedbackTrue = 'You noticed ' + this.catchEventDescription + ' and pressed "' + this.catchKeyText + '"!';
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
        if (typeof(this.reps) === 'undefined') {
            this.reps = this.stimuliObj.calibReps;
        }

        this.stims = [];
        for (var br = 0; br < this.blockReps; br++) {
            this.stims = this.stims.concat(orderStimuli(this.reps, this.stimuliObj.continuum.length, this.blockRandomizationMethod));
        }

        // get correct responses for (ordered) stimuli
        this.correctResponses = [];
        if (this.provideFeedback) {
          for (var i = 0; i < this.stims.length; i++) {
            this.correctResponses = this.correctResponses.concat(this.stimuliObj.mappingStimulusToCorrectResponse[this.stimuliObj.filenames[this.stims[i]]]);
          }
        }

        this.pbIncrement = 1.0 / this.stims.length;

        ////////////////////////////////////////////////////////////////////////////////
        // Bind handlers for this block:
        // create handler to capture and process keyboard input, and bind to document
        $(document).bind('keyup.' + this.namespace, function(e) { _self.handleResp(e); });

        ////////////////////////////////////////////////////////////////////////////////
        // Initialize UI elements
        // set task instructions and response cues
        $("#catchTrialInstruction").html(this.catchTrialInstruction);
        $("#catchTrialFeedbackTrue").html(this.catchTrialFeedbackTrue);

        $("#taskInstructions").html('Press <span id="bKey" class="respKey">' +
                                    valToKey(this.respKeys, this.categories[0]) +
                                    '</span> for "' + this.categories[0] + '"<br />' +
                                    'Press <span id="dKey" class="respKey">' +
                                    valToKey(this.respKeys, this.categories[1]) + '</span> for "' + this.categories[1] + '"');

        if (!softInit) {
            // install, initialize, and show a progress bar (progressBar.js)
            installPB("progressBar");
            resetPB("progressBar");
            $("#progressBar").show();
            // DEBUGGING: add button to force start of calibration block (skip preview)
            $('#buttons').append('<input type="button" onclick="calibrationBlock.next()" value="start calibration"></input>');
        }
    },

    // start next trial
    next: function() {
        var _self = this;

        $("#taskInstructions").show().addClass("dimmed");
        $("#catchTrialInstruction").show().addClass("dimmed");
        $("#catchTrialFeedbackTrue").hide();

        this.isCatchTrial = undefined;
        this.catchAns = false;
        this.stimulusFinished = false;

        // pause before next fixation cross appears
        setTimeout(function() {
                     $("#fixation").show();
                     }, _self.ITI/2);
        // play next stimuli after ITI has elapsed (asynchronously with fixation display)
        setTimeout(function() {
                         // NOTE: can't use 'this' here, since function is being evaluate outside of method context
                         var current = _self.auds[_self.stims[_self.n]];
                         throwMessage(current);
                         _self.waitForResp();

                         if (current.type == 'video') {
                            var video = document.createElement('video');
                            video.src = current.src;
                            video.setAttribute('class', current.class);
                            document.body.children.videoContainer.appendChild(video);

                            lastplayed = current.src.split("/").pop();
                            _self.isCatchTrial = lastplayed.split(".")[0].indexOf("CATCH") > -1;
                            video.play();
                            $(video).bind('ended.' + this.namespace, function() { _self.stimulusHasPlayed(); });
                            video = null;
                         }
                         if (current.type == 'audio') {
                            var audio = document.createElement('audio');
                            audio.src = current.src;
                            audio.setAttribute('class', current.class);
                            document.body.children.audioContainer.appendChild(audio);
                            audio.play();
                            lastplayed = current.src.split("/").pop();
                            $(audio).bind('ended.' + this.namespace, function() { _self.stimulusHasPlayed(); });
                            audio = null;
                         }
                         _self.tStart = Date.now();
                         $('#testStatus').html('Trial started');
                     }, _self.ITI);
    },

    // This function was introduced to separate questions about whether the stimulus has finished playing
    // from questions about whether keyCapture is on (See waitForResp). This makes it possible to decide
    // whether a key response is accepted prior to the video having finished.
    stimulusHasPlayed: function() {
        this.stimulusFinished = true;
        $("#fixation").hide();
        $("#taskInstructions").removeClass("dimmed");
        $('#testStatus').html('Stim ended');
    },

    waitForResp: function() {
        this.keyCapture = true;
    },

    handleResp: function(e) {
        throwMessage('keyup() detected: keycode = ' + e.which + ' (' +
                              String.fromCharCode(e.which) + ')');

        // add this.stimulusFinished as condition if keypress should only be accepted after video played.
        // (this is independent of whether the trial can *advance*, which is handled by the end function)
        if (this.keyCapture && e.which == this.catchKeyCode) {
            throwMessage("CATCH response detected.");
            $("#catchTrialInstruction").hide();
            $("#catchTrialFeedbackTrue").show();
            this.catchAns = true;

            this.tResp = Date.now();
            // If catchEndsTrial, no more keys are recorded (so catch response pre-empts other responses)
            if (this.catchEndsTrial) {
              this.keyCapture = false;
              this.handleFeedback(e);
            }
        }

        // add this.stimulusFinished as condition if keypress should only be accepted after video played.
        // (this is independent of whether the trial can *advance*, which is handled by the end function)
        if (this.respKeys[String.fromCharCode(e.which)]) {
          if (this.stimulusFinished && this.keyCapture) {
            this.tResp = Date.now();
            this.keyCapture = false;
            this.handleFeedback(e);
          } else if (!this.stimulusFinished && this.keyCapture) {
            this.stimulusFinished = true; // play 'please wait until end of stimulus' message only once

            alert('Please wait until the video has finished playing before you respond.\n\n' +
                  'Click OK and then respond again.');
          }
        }

    },

    handleFeedback: function(e) {
      // If no feedback is to be provided, end the trial
      if (!this.provideFeedback) {
        this.end(e);
        return -1;
      } else {
      // Feedback should be provided, so determine what that feedback ought to be
        var pressedKeyLabel = String.fromCharCode(e.which);
        if (pressedKeyLabel === ' ') pressedKeyLabel = "SPACE";

        // Determine what key response was and what that indicates.
        var feedbackString = "You pressed \"" + pressedKeyLabel + "\", indicating that this video contained ";
        if (this.catchAns) {
          feedbackString += this.catchEventDescription;
        } else {
          feedbackString += this.respKeys[String.fromCharCode(e.which)];
        }
        feedbackString += ". ";

        // If this was the correct response, provide positive feedback and end the trial
        if ((this.isCatchTrial && this.catchAns) || (!this.isCatchTrial && (this.respKeys[String.fromCharCode(e.which)] === this.correctResponses[this.n]))) {
          alert(feedbackString +  "This is CORRECT. Click OK to continue.");
          this.end(e);
          return -1;
        } else if (!this.isCatchTrial && this.catchAns) {
        // Subject wrongly indicated a catch trial
          feedbackString += "But this is INCORRECT: the video did NOT contain " + this.catchEventDescription +
            '. You should have pressed "' + valToKey(this.respKeys, this.correctResponses[this.n]) +
            '" to indicate that the video contained a ' + this.correctResponses[this.n] + ". ";
        } else if (this.isCatchTrial && !this.catchAns) {
          // Subject missed catch trial but correctly identified stimulus
          if (this.respKeys[String.fromCharCode(e.which)] === this.correctResponses[this.n]) {
            feedbackString += "This is NOT QUITE CORRECT. While the video indeed contained a " + this.correctResponses[this.n] +
              ", the video also contained " + this.catchEventDescription + ". ";
          } else {
          // Subject missed catch trial *and* incorrectly identified stimulus
            feedbackString += "This is INCORRECT. The video contained a " + this.correctResponses[this.n] +
            ", not a " + this.respKeys[String.fromCharCode(e.which)] + ". But it also contained " + this.catchEventDescription + ". ";
          }
          feedbackString += 'On such trials like this one, you should press "' + this.catchKeyText + '".';
        } else if (this.respKeys[String.fromCharCode(e.which)] !== this.correctResponses[this.n]) {
          // Subject correctly handled catch trial *but* incorrectly identified stimulus
          feedbackString += "This is INCORRECT. The video contained a " + this.correctResponses[this.n] + ". " +
            'On trials like this one, you should press "' + valToKey(this.respKeys, this.correctResponses[this.n]) + ".";
        } else {
          throwError("Some key event occurred that was not foreseen.");
        }

        alert(feedbackString + "\n\nMaking mistakes during practice is absolutely OK---that's why we have a practice phase. " +
        "Remember to listen closely and respond based on whether the video contains a real word of English or " +
        "not (except when the video has " + this.catchEventDescription + "). Press OK to continue.");

        this.handleMistake(e);
      }
    },

    handleMistake: function(e) {
      // If enforcePerfection is true then restart the whole block.
      if (!this.enforcePerfection) {
        this.end(e);
      } else {
        // record response here since otherwise it won't be recorded (since trial won't be ended)
        this.recordResp(e);

        alert("Since it is important that you understand your task before you continue to the main part of the experiment, " +
          "we will restart the practice phase. This will give you additional time to practice. Press OK to continue.");

        $('video').eq(0).remove();
        $('audio').eq(0).remove();

        this.init();
        this.next();
      }
    },

    // method to record response, called by end() at end of trial. takes event object as input
    recordResp: function(e) {

        // format trial information
        var resp = [this.info(), // the namespace, the trial number, stimulus, and video played
                    e.which, // keycode of key press
                    this.respKeys[String.fromCharCode(e.which)], // response key mapping
                    this.tStart, this.tResp, this.tResp-this.tStart, // timing information
                    this.isCatchTrial, this.catchAns].join(); // is this a catch trial and was the answer a catch response?
        throwMessage('Recording response: ' + resp);

        // write info to form field by concatening with already existing information
        //$('#calibrationResp').val($('#calibrationResp').val() + resp + RESP_DELIM);
        $(this.respField).val($(this.respField).val() + resp + RESP_DELIM);
    },

    // return info on current state in string form
    info: function() {
        return [this.namespace,
                this.n, this.stims[this.n], lastplayed].join();
    },

    // handle end of trial (called by key press handler)
    end: function(e) {
        // This gets rid of the problem of div creeping...
        $('video').eq(0).remove();
        $('audio').eq(0).remove();
        // update progress bar
        plusPB("progressBar", this.pbIncrement);
        this.recordResp(e);

        // if more trials remain, trigger next trial
        if (++this.n < this.stims.length) {
            this.next();
        } else {
            this.endBlock();
        }
    },

    endBlock: function() {
        // trigger endCalibrationBlock event
        $("#taskInstructions").hide();
        $("#catchTrialInstruction").hide();
        $("#catchTrialFeedbackTrue").hide();
        $("#progressBar").hide();
        $(this.auds).unbind('.' + this.namespace).height(0);
        $(document).unbind('.' + this.namespace);
        $(document).trigger('endBlock_' + this.namespace);
        if (typeof(this.onEndedBlock) === 'function') {
            this.onEndedBlock();
        } else {
            throwWarning('End of block reached but no callback found');
        }
    },

    demo: function() {
        // function to demonstrate categories.

    },

};



// classical-esque class inheritance: sets prototype of prototype to superclass prototype
function extend(child, supertype)
{
    child.prototype.__proto__ = supertype.prototype;
    child.prototype.__super__ = supertype.prototype;
}



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


function validateRespKeys(respKeys, categories) {
    for (k in respKeys) {
        if (! categories.has(respKeys[k])) {
            throwError('response label {0} not found in specified categories {1}'.format(respKeys[k], categories));
            return false;
        }
    }
    return true;
}
