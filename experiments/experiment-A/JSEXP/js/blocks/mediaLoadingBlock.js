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

function MediaLoadingBlock(params) {
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
        case 'ITI':
            this.ITI = params[p];
            break;
        case 'totalLoadingThreshold':
        	this.totalLoadingThreshold = params[p];
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
    }
    this.namespace = namespace;

    // install stimuli
    if (isArray(stimuliObj)) {
        // concatenate into one mega-object, and set for this block
        // NOTE / TODO: I'm uncertain if this part of the loop works... when would you need it?
        this.stimuliObj = concatenate_stimuli_and_install(stimuliObj, css_stim_class);
        this.media = this.stimuliObj.installed;
    } else {
        // set stimuli object for this block
        this.stimuliObj = stimuliObj;
        $('#continue').show();
    }

    // create responses form element and append to form
    this.respField = $('<textArea id="' + this.namespace + 'Resp" ' +
                       'name="' + this.namespace + 'Resp" ></textArea>').appendTo('#mturk_form');
    $('#mturk_form').append('<br />');

}


MediaLoadingBlock.prototype = {
    stimuli: [],
    n: 0,
    tBlockStart: -1, //time block starts
    tStartLoad: -1, //time at beginning of load
    tEndLoad: -1,   //time at end of load
    totalLoadingThreshold: 1000 * 60 * 10, // The time in ms after which loading will be considered an automatic strike
    ITI: -1,
    media: [],
    namespace: '',
    respField: undefined,
    onEndedBlock: undefined,
    pbIncrement: undefined,

    run: function() {
        var _self = this;
        _self.init();
       _self.next();
    },

    init: function(opts) {
        var _self = this;

        // initialize trial counter and stimulus order
        this.n = 0;
        this.stims = _.range(_self.stimuliObj.filenames.length);
        this.pbIncrement = 1.0 / this.stims.length;

        // Initialize UI elements
        installPB("progressBar");
        resetPB("progressBar");
        $("#progressBar").show();

        _self.tBlockStart = Date.now();
    },

    // start next trial
    next: function() {
        var _self = this;
        var suffix;
        if (_self.stimuliObj.mediaType === 'audio') {
          suffix = audSuffix;
        } else if (_self.stimuliObj.mediaType === 'video') {
          suffix = vidSuffix;
        } else {
          suffix = '';
        }

        // If the filename already has the extension in it, don't put it in
        var current = _self.stimuliObj.prefix + _self.stimuliObj.filenames[_self.stims[_self.n]];
        if (_self.stimuliObj.filenames[_self.stims[_self.n]].indexOf(suffix) == -1) {
          current = current + suffix;
        }
        $("#taskInstructions").text("Loading files: " + _self.n + " / " + _self.stims.length);
        $("#taskInstructions").show();

        // Immediately plays
        setTimeout(function() {
          if (_self.stimuliObj.mediaType === 'audio') {
            $originalMedia = $('<audio>');
          } else {
            $originalMedia = $('<video>');
          }
          _self.tStartLoad = Date.now();

          $originalMedia.attr("src", current);
          $originalMedia.attr("preload", "auto");

          $originalMedia.on("canplay", function() {
            _self.tEndLoad = Date.now();
            // Wait 50 ms just to be careful?
            setTimeout(function(){_self.end();}, 50);
          });
        }, _self.ITI);  // wait until ITI done before next trial
    },

    // handle end of trial
    end: function() {
        _self = this;
        $("#taskInstructions").empty();

        // update progress bar
        plusPB("progressBar", _self.pbIncrement);
        // record response
        _self.recordResp();
    },

    // method to handle response. takes event object as input
    recordResp: function() {
        // store load time
        $(this.respField).val($(this.respField).val() + (this.tEndLoad - this.tStartLoad) + RESP_DELIM);

        // if more trials remain, trigger next trial
        if (++this.n < this.stims.length) {
          this.next();
        } else {
          this.endBlock();
        }
        return;
    },

    endBlock: function() {
        // trigger endCalibrationBlock event
        $('#fixation').hide();
        $("#taskInstructions").hide();
        $("#progressBar").hide();
        $("#instructionsLower").hide();
        $("#fileloadInformation").hide();
        $("#wrongAnsMessage").hide();
        $("#CatchFeedback").hide();

        this.tBlockEnd = Date.now();

        throwMessage("Full loading time: " + (this.tBlockEnd - this.tBlockStart));
        if (this.totalLoadingThreshold > 0) {
        	  if (this.tBlockEnd - this.tBlockStart > this.totalLoadingThreshold) {
        		    alert("It seems that your internet connection (or our server) is too slow right now. This will likely lead to problems during the experiment.\n\nWe would appreciate if you could return the HIT, and apologize for the inconvenience.");
			      }
		    }

        $(document).off();
        if (typeof(this.onEndedBlock) === 'function') {
            this.onEndedBlock();
        } else {
            if (console) throwWarning('End of block reached but no callback found');
        }
    }

}
