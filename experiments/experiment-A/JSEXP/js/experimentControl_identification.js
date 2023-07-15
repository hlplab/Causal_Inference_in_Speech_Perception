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

var vidSuffix, audSuffix;


// update experimental control script.  defines Experiment object.

// convenience variable for debugging (block of experiment currently being executed)
var _curBlock;

function Experiment(baseobj) {
    // add any properties passed as object to this, overriding defaults
    $.extend(this, baseobj);
    this.consentFormDiv = '<div id="consent">By accepting this HIT, you confirm that you have read and understood the <a target="_blank" href="' + baseobj.rsrbConsentFormURL +
        '">consent form</a>, that you are willing to participate in this experiment, and that you agree that the data you provide by participating can be used in scientific publications (no identifying information will be used). Sometimes it is necessary to share the data elicited from you &mdash; including sound files &mdash; with other researchers for scientific purposes (for replication purposes). That is the only reason for which we will share data and we will only share data with other researchers and only if it is for non-commercial use. Identifying information will <span style="font-weight:bold;">never</span> be shared (your MTurk ID will be replaced with an arbitrary alphanumeric code).</div>'
}

Experiment.prototype = {
    // defaults
    blocks: [],
    blockn: undefined,
    cookie: 'alreadyDoneCookie',
    rsrbProtocolNumber: 'RSRB00045955',
    rsrbConsentFormURL: 'https://www.hlp.rochester.edu/consent/RSRB45955_Consent_2021-02-10.pdf',

    // addBlock: function(block, instructions, endedHandler, practiceParameters) {
    addBlock: function(obj) {
        var block, instructions, endedHandler, practiceParameters, onPreview;
        // detect "naked block" vs. object with block info
        if (typeof(obj.run) === 'function' || typeof(obj) === 'function') {
            // naked block cases:
            // naked blocks are either objects with a .run() method (first case)
            // or functions themselves (which are called by Experiment.nextBlock()
            // and return a block object)
            block = obj;
        } else {
            // block + parameters objects, with fields:
            block = obj['block'];
            instructions = obj['instructions'];
            endedHandler = obj['endedHandler'];
            practiceParameters = obj['practiceParameters'];
            // show block during preview?
            onPreview = typeof(obj['onPreview']) === 'undefined' ?
                false :
                obj['onPreview'];
        }

        // add onEndedBlock handler function to block (block object MUST
        // call its onEndedBlock method  when it has truly ended...)
        var _self = this;
        block.onEndedBlock =
            typeof(endedHandler) === 'undefined' ?
            function() {_self.nextBlock();} :
            endedHandler;
        // and link back to this experiment object to block object...
        block.parent = _self;
        // add block object and its associated instructions to the blocks array
        this.blocks.push({block: block,
                          instructions: instructions,
                          practiceParameters: practiceParameters && practiceParameters.substring ?
                          {instructions: practiceParameters} : practiceParameters,
                          onPreview: onPreview}); // gracefully handle bare instructions strings
    },

    nextBlock: function() {
        // pull out block object holder, but don't increment block counter yet
        scroll(0,0);
        var this_block = this.blocks[this.blockn];
        if (typeof(this_block) === 'undefined') {
            // no more blocks, so finish up
            this.wrapup();
        } else {
            // check for preview mode, and stop if not ready.
            if (e.previewMode && !this_block.onPreview) {
                $("#continue").hide();
                $("#instructions").html('<h3>End of preview </h3><p>You must accept this HIT before continuing</p>').show();
                return false;
            }

            // if the block is given as a function, evaluate that function to create real block
            if (typeof this_block.block === 'function') {
                // functions should take a callback as first argument.
                this_block.blockfcn = this_block.block;
                // ... and return a block object.
                this_block.block = this_block.blockfcn(this_block.block.onEndedBlock);
            }

            _curBlock = this_block.block;
            var _self = this;

            // then check to see if practice mode is needed.
            if (typeof this_block.practiceParameters !== 'undefined') {
                // if yes, do practice mode, with a call back to run the block for real
                this_block.block.practice(this_block.practiceParameters,
                                          function() {_self.runBlock();});
            } else {
                // otherwise, run the block for real.
                this.runBlock();
            }
        }

    },

    // method to actually RUN the current block, showing optional instructions if they're provided
    runBlock: function() {
        var this_block = this.blocks[this.blockn++];
        var _self = this;

        if (typeof(this_block.instructions) !== 'undefined') {
            // if there are instructions...
            // show them, with a continue button
            $("#instructions").html(this_block.instructions).show();
            continueButton(function() {
                               $("#instructions").hide();
                               //this_block.block.run();
                               //_curBlock = this_block.block;
                               this_block.block.run();
                           });
        } else {
            // ...otherwise, just run the block.
            this_block.block.run();
        }
    },

    init: function() {
        this.blockn = 0;

        // read in URL parameters
        this.urlparams = gupo();
        // Determine whether the experiment is run in debug mode. This activates several shortcuts through
        // the experiment and makes otherwise invisible information visible. Set URL param debug=TRUE.
        this.debugMode = checkDebug(this.urlparams);
        if (this.debugMode) console.log("Entering VERBOSE (debugging) mode.");
        // Determine whether the experiment is run in preview mode.
        // Preview is what MTurkers see prior to accepting a HIT. It should not contain any information
        // except for the front page of the experiment.
        // If URL parameter assignmentId=ASSIGNMENT_ID_NOT_AVAILABLE the preview mode will be selected.
        this.previewMode = checkPreview(this.urlparams);
        // Determine whether the experiment is run in sandbox mode. This is inferred not from the URL
        // parameters but automatically from the documents' referrer. I.e., this function recognizes
        // if the HTML is embedded in an MTurk sandbox iframe (the argument is optional, it seems).
        this.sandboxmode = checkSandbox();

        // get assignmentID and populate form field
        $("#assignmentID").val(this.urlparams['assignmentID']);
        this.randomID = randomString(16, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ');
        // record random ID for this instance of the experiment
        $("#randomID").val(this.randomID);

        // record userAgent
        $("#userAgent").val(navigator.userAgent);
        // Clients' UTC time offset (in minutes) is added as userDateTimeOffset o form just before final
        // submit in function mturk_end_surveys_and_submit() in mturk_helpers (so as to have the timezone
        // offset just before submit).

        // Record all url param fields
        for (param in this.urlparams) {
                $('<input>').attr({
                    type: 'hidden',
                    id: param,
                    name: param,
                    value: this.urlparams[param]
                }).appendTo('form#mturk_form');
        }

        // detect whether the browser can play audio/video and what formats
        vidSuffix =
            Modernizr.video.webm ? '.webm' :
            Modernizr.video.h264 ? '.mp4' :
            '';
        audSuffix =
            Modernizr.audio.wav == 'probably' ? '.wav' :
            Modernizr.audio.ogg == 'probably' ? '.ogg' :
            Modernizr.audio.mp3 == 'probably' ? '.mp3' :
            Modernizr.audio.wav == 'maybe' ? '.wav' :
            '';

        var IE = (!! window.ActiveXObject && +(/msie\s(\d+)/i.exec(navigator.userAgent)[1])) || NaN;
        var Safari = NaN;
        if (navigator.userAgent.includes('Safari/') && ! navigator.userAgent.includes('Chrome/'))
            Safari = parseInt(navigator.userAgent.split('Version/')[1].split('.')[0]);

        // Browser must be IE version 10 or higher, Safari version 13 or lover, or Chrome or Opera
        if (IE < 9 || Safari > 13 || navigator.userAgent.includes('Firefox/')) {
          $("#errorMessage").show();
          $("#instructions").hide();
          throwWarning("Incompatible browser detected");
          return false;
        }

        // check for video and audio support, and if it's missing show a message
        // with an explanation and links to browser download websites
        if (vidSuffix && audSuffix) {
          $("#errorMessage").hide();
          $("#instructions").show();
        } else {
          $("#errorMessage").show();
          $("#instructions").hide();
          throwWarning("Browser can't play both audio and video formats.");
          return false;
        }

        var cookie = readCookie(this.cookie);
        if (!this.sandboxmode && !this.debugMode && cookie) {
          $("#errorMessage").html('<p>It looks like you have already completed this HIT (or reloaded this HIT) or a similar version of this HIT.</p><p>Please <strong>do not accept this HIT</strong>, your results will be automatically rejected.</p><p>If you accidentally reloaded this HIT, please do not continue. Instead, please <a target="_blank" href="mailto: hlplab@gmail.com">email us</a>.</p>');
          $("#errorMessage").show();
          $("#instructions").hide();
          throwWarning("Cookie detected.");
          return false;
        }
        createCookie(this.cookie, 1, 365);

        // format consent form div with appropriate link to consent form.
        this.consentFormDiv = this.consentFormDiv.format(this.rsrbConsentFormURL);

        // populate remaining DIVs now that all incompatible browsers have been handled
        $("#continue").html('<span id="contText">Click to continue...</span>');
        $("#fixation").html('+');

        // load post-experiment survey into div in form
        $('form#mturk_form')
            .append($('<div id="endForm" class="survey"></div>')
                    .load(this.survey + ' #endForm > *'));

        // set up form for end of experiment with demographic and other info
        // load demographic survey into div in form
        var rsrbNum = this.rsrbProtocolNumber;
        $('form#mturk_form')
            .append($('<div id="rsrb" class="survey">')
                    .load('JSEXP/surveys/rsrb_survey.html #rsrb > *', function() {
                        // set protocol number
                        $('input[name="rsrb.protocol"]:hidden').val(rsrbNum);
                        throwMessage('Name of RSRB protocol: ' + rsrbNum + '\nRSRB information written into form field: ' + $('input[name="rsrb.protocol"]').val() + "\n(this value is expected to be undefined unless you are sandboxing or live)");
                    }));

    },

    wrapup: function(why) {
        throwMessage("Wrapping up experiment.");

        if (typeof(why) === 'undefined') {
          // success
          // no error reported to callback
          $("#instructions").html("<h3>Thank you for participating in this experiment.</h3>" +
          "<p>There are just a few more questions we ask you to answer.</p>")
          .show();

          // mturk_end_surveys_and_submit() is a function in js-adapt/mturk-helpers.js
          // which steps through the demographics (RSRB) and post-experiment surveys and then submits.
          continueButton(mturk_end_surveys_and_submit);
        } else {
            // error?
            // any parameter not undefined is assumed to be an error, so record it and then wrap up.
            $("#experimentStatus").append("wrapup called: " + why + "<br />");
            $("#errors").val($("#errors").val() + why + RESP_DELIM);
            $("#instructions").html("<h3>Experiment is over</h3>" +
                                    "<p>Unfortunately, we were not able to calibrate the experiment to your hearing and audio system, and this is the end of the experiment.  If you have any comments, please write them in the box below before submitting this HIT.  Thank you for participating.</p>")
                .show();
        }

    }
};
