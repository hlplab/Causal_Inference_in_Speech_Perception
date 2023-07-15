/*
 * Author: Dave F. Kleinschmidt
 * http://davekleinschmidt.com
 *
 *    Copyright 2013 Dave Kleinschmidt and
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
 *
 * visualGridBlock.js: javascript code for implementing a visual world experiment where
 * stimulus images are displayed and can then be clicked on in response to an audio
 * stimulus.
 */

function VisualGridBlock(params) {
    for (p in params) {
        switch(p) {
        case 'stimuli':
            this.stims = params[p];
            break;
        case 'instructions':
            instructions = params[p];
            break;
        case 'namespace':
            namespace = params[p];
            break;
        case 'allowFeedback':
            this.allowFeedback = params[p];
            break;
        case 'autoAdvanceReady':
            this.autoAdvanceReady = params[p];
            break;
        case 'ITI_trialStartToImages':
            this.ITI_trialStartToImages = params[p];
            break;
        case 'ITI_imagesToAudioStart':
            this.ITI_imagesToAudioStart = params[p];
            break;
        case 'ITI_responseToTrialEnd':
            this.ITI_responseToTrialEnd = params[p];
            break;
        case 'OnNegativeFeedback_blinkInterval':
            this.OnNegativeFeedback_blinkInterval = params[p];
            break;
        case 'OnNegativeFeedback_blinkNumber':
            this.OnNegativeFeedback_blinkNumber = params[p];
            break;
        case 'images':
            this.images = params[p];
            break;
        case 'imageMapping':
            this.imageMapping = params[p];
            break;
        case 'imagePositions':
            this.imagePositions = params[p];
            break;
        case 'randomizeImagePositions':
            this.randomizeImagePositions = params[p];
            break;
        case 'breakEvery':
            this.breakEvery = params[p];
            break;
        case 'showFamiliarization':
            this.showFamiliarization = params[p];
            break;
        case 'randomizationMethod':
            this.randomizationMethod = params[p];
            break;
        case 'debugMode':
            this.debugMode = params[p];
            break;
        default:
            if(console) console.log('Warning: unknown parameter passed to visualGridBlock: ' + p);
            break;
        }
    }

    this.n = 0;

    // set namespace for this block (prefix for events/form fields/etc.) and
    // the class that the stimuli are assigned
    if (typeof(namespace) === 'undefined') {
        this.namespace = 'visualGrid';
    } else {
        this.namespace = namespace;
    }

    // set intructions for this bloc
    if (typeof(instructions) === 'undefined') {
        this.instructions = '<h3>Start of experiment</h3>'+
              '<p>The pictures and words you just saw will be used in the next phase of the experiment. First you will see two pictures and a green circle. When the green circle lights up, click on the circle. You will then hear a word. Please click on the picture that you hear.</p><p><strong>Please respond as quickly and as accurately as possible.</strong>  If you\'re not sure, please take your best guess. ' +
              'The progress bar at the top will show you how many trials you have completed and how many trials remain.</p>';
    } else {
        this.instructions = instructions;
    }

    // add images to DOM
    for (image_name in this.images) {
        $('<img />')
            .addClass(this.namespace + 'image')
            .attr('id', image_name)
            .attr('src', this.images[image_name])
            .trigger('load')
            .hide()
            .appendTo('#visualGridContainer');
    }

    // Create audio file item
    $("<audio />")
        .addClass(this.namespace + 'audio')
        .attr('preload', 'auto')
        .appendTo('#visualGridContainer');

    // create response form fields
    this.respField = $('<textArea id="' + namespace + 'Resp" ' +
                       'name="' + namespace + 'Resp" ></textArea>').appendTo('#mturk_form');
    $('#mturk_form').append('<br />');
}

VisualGridBlock.prototype = {
    allowFeedback: false,
    autoAdvanceReady: false,
    itemOrder: undefined,       // replaces this.stims in LabelingBlock, indexed by n, indexes stimuli
    randomizationMethod: 'shuffle',
    randomizeImagePositions: true,
    imagePositions: ['topleft', 'topright', 'bottomleft', 'bottomright'],
    ITI_trialStartToImages: 1000,  // time from trial start to showing pictures
    ITI_imagesToAudioStart: 2000,  // time from trial to start to audio play (only relevant if autoAdvanceReady == T)
    ITI_responseToTrialEnd: 2000,
    OnNegativeFeedback_blinkInterval: 400, // how long is the blink on and off for (if it's shown)?
    OnNegativeFeedback_blinkNumber: 8,     // How many blinks are shown? (each blink takes blinkInterval ms)
    breakEvery: 100,            // number of trials between breaks
    trialsPerMinute: 12.5,      // NOT YET TESTED. number of trials per minute (based on testing, ~500 trials in 40 mins)
    clickCapture: false,
    onEndedBlock: undefined,
    showFamiliarization: true,
    debugMode: false,

    run: function() {
        var _self = this;
        this.init();
        if (_self.showFamiliarization == true) {
            _self.familiarize();
        }
        else {
            _self.endFamiliarize();
        }
    },

    init: function() {
        var _self = this;

        // initialize trial counter
        this.n = 0;

        // install wait/go images
        $('<div id="readyWaitContainer"> </div>').appendTo('#visualGridContainer');
        $("<img />")
            .addClass('visualGrid')
            .attr('id', 'ready')
            .attr('src', 'JSEXP/img/greenready.png')
            .appendTo('#readyWaitContainer')
            .hide()
            .trigger('load');
        $("<img />")
            .addClass('visualGrid')
            .attr('id', 'wait')
            .attr('src', 'JSEXP/img/greenwait.png')
            .appendTo('#readyWaitContainer')
            .hide()
            .trigger('load');


        ////////////////////////////////////////////////////////////////////////////////
        // construct list of items and randomize trial order
        // NOT SURE ABOUT THIS TODO
        this.itemOrder = pseudoRandomOrder(this.stims.reps, undefined, this.randomizationMethod);

        // If trials are NOT to auto-advance from the ready to play state,
        // install "start trial" handler for the "ready" light
        if (!this.autoAdvanceReady) {
          $('#readyWaitContainer img#ready')
            .click(function() {
                       // "turn off" the light
                       $(this).hide().siblings('img#wait').show();
                       // play stimulus and wait for response
                       _self.handlePlay();
                   });
        }

        // install click handler on the stimulus images
        $('img.' + this.namespace + 'image').click(function(e) {_self.handleResp(e);});

        // install, initialize, and show a progress bar (progressBar.js)
        installPB("progressBar");
        resetPB("progressBar");
        $("#progressBar").show();
        this.pbIncrement = 1.0 / this.itemOrder.length;
    },

    takeBreak: function() {
        var _self = this;
        $("#visualGridContainer").hide();
        $("#instructions").html('<h3>Break Time!</h3><p>If you\'d like to take a break, you can do that now. Keep in mind that you have a limited amount of time to complete this HIT.</p>').show();
        continueButton(function() {
                           $("#instructions").hide();
                           $("#visualGridContainer").show();
                           _self.next();
                       });
    },

    next: function() {
        var _self = this;

        $('#readyWaitContainer img#wait').show();
        // after ITI_trialStartToImages, turn on "ready" light, and display images
        setTimeout(function() {
                       $('#readyWaitContainer img#wait').hide().siblings('img#ready').show();
                       _self.showStimImages();
                   }, _self.ITI_trialStartToImages);

        // If trials ARE to auto-advance from the ready to play state,
        // initiate the play state after ITI_imagesToAudioStart
        if (this.autoAdvanceReady) setTimeout(function() {
          _self.handlePlay(); }, _self.ITI_imagesToAudioStart);
    },

    handlePlay: function() {
        throwMessage('Playing stimulus (in handle).');
        $('.'+this.namespace + 'audio').trigger('play');
        this.waitForResp();
        this.tStart = Date.now();
    },

    showStimImages: function() {
        throwMessage('Showing pictures.');
        var _self = this;
        var positions;
        if (this.randomizeImagePositions == true) {
            positions = shuffle(this.imagePositions);
        } else {
            positions = this.imagePositions;
        }

        // Load the relevant sound file while the images are being shown
        $('.' + this.namespace + 'audio')
            .attr('id', 'trialAudio')
            .attr('src', this.stims.prefix + this.stims.filenames[this.itemOrder[this.n]])
            .trigger('load');

        // Get the image mapping for the current trial
        var currentStimMapping = this.imageMapping[this.stims.image_selections[this.itemOrder[this.n]]];
        $.map(currentStimMapping[this.stims.target_words[this.itemOrder[this.n]]],
              function(image, i) {
                  $('img#' + image + '.' + _self.namespace + 'image')
                      .addClass('vw_trialimage')
                      .attr('vw_pos', positions[i])
                      .show();
              });
    },

    waitForResp: function() {
        throwMessage('Waiting for response.');
        // if collecting a keyboard response, would turn on listening here
        this.clickCapture = true;
    },

    handleResp: function(e) {
        throwMessage('Handle response.');
        if (this.clickCapture) {
            this.tResp = Date.now();
            this.clickCapture = false;

            // switch off the green light if it's still on
            if (this.autoAdvanceReady) $('#readyWaitContainer img#ready').hide().siblings('img#wait').show();
            this.handleFeedback(e);
        }
    },

    info: function() {
        // // pull out stimulus file basename for current trial
        // var curStimSrc = this.stims.filenames[this.itemOrder[this.n]];
        // Go over the stimulus list fields, and extract information about the current trial
        // from all (and only) fields that are objects (the arrays).
        var currentStimuliInfo = [];
        for (v in this.stims) {
            if (typeof this.stims[v] === 'object') {
                // Extract the information for the present item
                currentStimuliInfo.push(this.stims[v][this.itemOrder[this.n]]);
            }
        }
        return [this.namespace, this.allowFeedback, this.n, this.itemOrder[this.n], currentStimuliInfo].join();
    },

    recordResp: function(e) {
        throwMessage('Record response.');
        var clickID, clickVWPos, clickVWx, clickVWy;
        clickID = e.target.id;                   // ID of element clicked
        clickVWPos = $(e.target).attr('vw_pos'); // vw_pos attr value of element clicked
        clickVWx = e.pageX - $("#visualGridContainer")[0].offsetLeft;
        clickVWy = e.pageY - $("#visualGridContainer")[0].offsetTop;
        var resp = [this.info(), clickID, clickVWPos, clickVWx, clickVWy,
                    this.tStart, this.tResp, this.tResp-this.tStart].join();
        $(this.respField).val($(this.respField).val() + resp + RESP_DELIM);
        // console.log(resp);
    },

    handleFeedback: function(e) {
      throwMessage('Handle feedback.');
      var _self = this;
      var currentStimMapping = this.imageMapping[this.stims.image_selections[this.itemOrder[this.n]]];
      var delayEnd = this.ITI_responseToTrialEnd;

      if (typeof(this.stims.feedback) === 'undefined') throwError("Feedback for this trial not defined.");

      // Are all conditions met to provide feedback?
      if (this.allowFeedback && this.stims.feedback[this.itemOrder[this.n]]) {
        var wrongAnswer = e.target.id !== this.stims.target_words[this.itemOrder[this.n]];
        // duration of visual blink (if click was wrong)
        var delayBlink = 0 + wrongAnswer * this.OnNegativeFeedback_blinkNumber * this.OnNegativeFeedback_blinkInterval;
        // time until correct audio is played again
        var delayReplay = 250 + delayBlink;
        delayEnd = delayEnd + delayReplay;

        // hide all images except the target
        $.map(currentStimMapping[this.stims.target_words[this.itemOrder[this.n]]],
            function(image, i) {
              if (image != _self.stims.target_words[_self.itemOrder[_self.n]]) {
                $('img#' + image + '.' + _self.namespace + 'image')
                    .hide();
                  }
            });

        if (wrongAnswer) {
          throwMessage("Blinking target at time " + Date.now());
          // COULD ADD BELL RING HERE?
          blinkId(this.stims.target_words[this.itemOrder[this.n]], this.OnNegativeFeedback_blinkInterval / 2, this.OnNegativeFeedback_blinkNumber);
        }

        setTimeout(function() {
          throwMessage("Playing audio again at time " + Date.now());
          document.getElementById('trialAudio').currentTime = 0;
          document.getElementById('trialAudio').play(); }, delayReplay);

      } else {
        // hide all images except the clicked one
        $.map(currentStimMapping[this.stims.target_words[this.itemOrder[this.n]]],
            function(image, i) {
              if (image != e.target.id) {
                $('img#' + image + '.' + _self.namespace + 'image')
                    .hide();
                  }
            });
      }

      throwMessage(
        "Current trial: " + _self.n + "\n" +
        "Current item: " + _self.itemOrder[_self.n] + "\n" +
        "Allow feedback in this block? " + _self.allowFeedback + "\n" +
        "Provide feedback on this trial? " + _self.stims.feedback[_self.itemOrder[_self.n]] + "\n" +
        "Answer: " + e.target.id + "\n" +
        "Correct answer: " + _self.stims.target_words[_self.itemOrder[_self.n]] + "\n" +
        "Was the anwer wrong? " + wrongAnswer + "\n" +
        "Delay blink: " + delayBlink + "\n" +
        "Delay replay: " + delayReplay + "\n" +
        "Delay end of trial: " + delayEnd);

      setTimeout(function() {
        throwMessage("Ending trial at time " + Date.now());
        _self.end(e); }, delayEnd);
    },

    end: function(e) {
      var _self = this;
      var delay = 0;
      // update progress bar
      plusPB("progressBar", this.pbIncrement);

      // record response
      this.recordResp(e);

      setTimeout(function() {
          // hide images and scrub of identifiers
          $('img.vw_trialimage')
          .removeClass('vw_trialimage')
          .removeAttr('vw_pos')
          .hide();

          // next trial, or end
          if (++_self.n < _self.itemOrder.length) {
            if (_self.n % _self.breakEvery == 0) {
              _self.takeBreak();
            } else {
              _self.next();
            }
          } else {
            _self.endBlock();
          }
      }, delay);
    },

    endBlock: function() {
        $("#visualGridContainer").hide();
        $("#progressBar").hide();
        $('#readyWaitContainer').remove();

        // finally: hand control back to whatever called this
        if (this.practiceMode && typeof(this.onEndedPractice) === 'function') {
            // handle callback provided for end of practice phase
            this.onEndedPractice();
        } else if (typeof(this.onEndedBlock) === 'function') {
            // will be set by Experiment if added as block
            this.onEndedBlock();
        } else {
            // otherwise, write warning to console.
            if (console) console.log('WARNING: End of block reached but no callback found');
        }
    },

    // run a familiarization block with the images+labels
    familiarize: function() {
        // show image + name, wait for click, then do next.
        // add a click handler to each image, which
        //   1) removes the handler
        //   2) hides the image
        //   3) shows the next image

        var _self = this;

        // iterate over images in random order, assigning handlers
        var imgs = shuffle($("."+this.namespace + 'image'));
        console.log(imgs);

        $("#progressBar").hide();
        $('#visualGridContainer').hide();
        $('#instructions')
            .html('<h3>Pictures and names</h3><p>Welcome to the experiment. First there will be a short familiarization phase. You will see a picture and its name.</p> <p>Please read the name and then click on the picture to see the next picture. These pictures will be used in the next phase of the experiment.</p>')
            .show();

        $(imgs)
            .addClass('familiarizationImage')
            .map(function(i, img) {
                        $(imgs[i]).bind('click.familiarization',
                                        function(e) {
                                            $(this).hide();
                                            // deal with final image
                                            if (i+1==imgs.length) {
                                                $(imgs)
                                                    .removeClass('familiarizationImage')
                                                    .unbind('.familiarization');
                                                $('#familiarizationText').remove();
                                                $('#familiarizationInstructions').remove();
                                                _self.endFamiliarize();
                                            } else {
                                                $(imgs[i+1]).show();
                                                $('#familiarizationText').html(imgs[i+1].id);
                                            }
                                        });
                    });

        // on continue click, start familiarization by showing first stim
        continueButton(function() {
                           $("#instructions").hide();
                           $("#visualGridContainer").show();
                           $(imgs[0]).show();
                           $('<div id="familiarizationInstructions"></div>')
                               .html('<p>Read the name, then click the image to advance</p>')
                               .prependTo('#visualGridContainer');
                           $('<div id="familiarizationText"></div>')
                               .html(imgs[0].id)
                               .appendTo("#visualGridContainer");
                       });
    },

    endFamiliarize: function() {
        if (console) console.log('Familiarization completed');
        $("#visualGridContainer").hide();
        var numTrials = this.itemOrder.length;
        // approximate duration of whole section, to nearest five minutes (rounded up)
        var timeNearestFiveMins = Math.ceil(this.itemOrder.length/this.trialsPerMinute / 5)*5;
        $("#instructions")
            .html(this.instructions)
            .show();

        var _self = this;
        continueButton(function() {
                           $("#progressBar").show();
                           $("#instructions").hide();
                           $("#visualGridContainer").show();
                           _self.next();
                       });
    },

};




function blinkId(id, interval, times) {
	var elem = document.getElementById(id);
  var count = 1;

  var intervalId = setInterval(function() {
    if (elem.style.visibility == 'hidden') {
      console.log("blinkiblink");
        elem.style.visibility = 'visible';
        // increment counter when showing to count # of blinks and stop when visible
        if (count++ === times) { clearInterval(intervalId); }
    } else {
        elem.style.visibility = 'hidden';
    }
  }, interval);
}
