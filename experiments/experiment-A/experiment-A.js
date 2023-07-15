/*
 * Author: Dave Kleinschmidt
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
 * Modified by: Linda Liu (2018)
 *
 *    The specific code in this js file was used to run the experiments in
 *    Liu and Jaeger (2018). Exposure is split into two blocks. This allows
 *    to present, e.g., causally ambiguous evidence in the first block,
 *    followed by causally disambiguating evidence in the second block.
 *
 * Modified by: Gevher Karboga and Florian Jaeger (2020)
 *
 *    The code was further modified for the replication of Liu and Jaeger (2018).
 *    For Experiment A of this replication, only one block was used. Both exposureStim
 *    and test lists are now generated on-line using constrained randomization.
 *    (see custom_functions.js; the previous methods if off-line generated lists
 *    is still available in the same file).
 */

// Variables defined here are globally visible
var _curBlock;
var vidSuffix, audSuffix;

// Experiment object to control everything
var e;
var RESP_DELIM = ';';

// global variable for computing the size for each increment of the progress bar (see progressBar.js)
var pbIncrementSize;


$(document).ready(function() {
    ////////////////////////////////////////////////////////////////////////
    // Create experiment
    ////////////////////////////////////////////////////////////////////////
    e = new Experiment(
        {
            rsrbProtocolNumber: 'RSRB00045955',
            rsrbConsentFormURL: 'https://www.hlp.rochester.edu/mturk/consent/RSRB45955_Consent_2023-01-11.pdf',
            survey: 'surveys/post_survey.html',
            cookie: 'CISP',
            requiredURLparams: ['label', 'respKeyTest']
        }
    );
    e.init();

    ////////////////////////////////////////////////////////////////////////
    // Parse relevant URL parameters -- DEBUG MODE
    ////////////////////////////////////////////////////////////////////////
    // e.urlparams is a dictionary (key-value mapping) of all the url params.
    // you can use these to control any aspect of your experiment you wish on a HIT-by-HIT
    // basis

    // You can use the following parameter to skip parts of the experiment for debugging:
    var skipTo = e.urlparams['skipTo'];              // [i]instructions, pre-[l]oad, p[ractice], e[xposure], t[est], s[urvey]

    ////////////////////////////////////////////////////////////////////////
    // Parse relevant URL parameters -- automatically inferred
    ////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////
    // Parse relevant URL parameters -- USER DEFINED
    ////////////////////////////////////////////////////////////////////////
    // is the pen in the hand or mouth during critical shifted trials?
    var condition = e.urlparams['condition'];         // M or H
    throwMessage('condition set to: '.concat(condition));
    if ($.inArray(condition, ['M', 'H']) < 0 && typeof(condition) !== 'undefined') throwError('Unrecognized condition.');
    // is s or sh shifed during exposure?
    // for all_typical, both s and sh are always typicall
    // for no_critical, only fillters are presented (not yet implemented)
    // for no_exposure, exposure is skipped completely. Payment and instructions are adjusted accordingly.
    var label = e.urlparams['label'];                 // S or SH (or all_typical or no_critical or no_exposure)
    throwMessage('label set to: '.concat(label));
    if ($.inArray(label, ['practice_only', 'no_exposure', 'no_critical', 'all_typical', 'S', 'SH']) < 0) throwError('Unrecognized label condition.');
    // which exposure and which test sets should be used?
    // We used exposure set A in NORM-A, NORM-B, and Exp A. Then we switched to exposure set B (see write-up)
    // We originally used test set A in NORM-A and then revised test set B in NORM-B and Exp A (see write-up)
    // Test set B was intended to change the audio steps to be more SH-eliciting and exchanged one
    // videoOriginalSound because it contained strong labeling information based on the first norming
    // experiments. However, we accidentally shifted the continuum in the wrong direction, and so we
    // created test set C, used in NORM-C, NORM-D, and Experiment B.
    var expSet = e.urlparams['expSet'];             // A (default) or B
    if (typeof expSet === 'undefined') expSet = 'A';
    expSet = expSet.toUpperCase();
    throwMessage('expSet set to: '.concat(expSet));

    var testSet = e.urlparams['testSet'];             // A (default) or B, C
    if (typeof testSet === 'undefined') testSet = 'A';
    testSet = testSet.toUpperCase();
    throwMessage('testSet set to: '.concat(testSet));

    // Should practice trials provide feedback? And, if so, should mistakes result in the practice session
    // being repeated?
    var practFeedback = e.urlparams['practFeedback'];             // false (default) or true
    if (typeof practFeedback === 'undefined') {
      practFeedback = false;
    } else {
      practFeedback = practFeedback.toLowerCase();
      if (practFeedback === 'f' || practFeedback === 'false') {
        practFeedback = false;
      } else if (practFeedback === 't' || practFeedback === 'true') {
        practFeedback = true;
      } else throwError('Unrecognized practFeedback.');
    }

    var practEnforcePerfection = e.urlparams['practEnforcePerfection'];             // false (default) or true
    if (typeof practEnforcePerfection === 'undefined') {
      practEnforcePerfection = false;
    } else {
      practEnforcePerfection = practEnforcePerfection.toLowerCase();
      if (practEnforcePerfection === 'f' || practEnforcePerfection === 'false') {
        practEnforcePerfection = false;
      } else if (practEnforcePerfection === 't' || practEnforcePerfection === 'true') {
        practEnforcePerfection = true;
      } else throwError('Unrecognized practEnforcePerfection.');
    }

    // Starting at NORM-D, we added the option of having a preloading block that loads all stimuli prior to
    // practice, exposure and test.
    var preloading = e.urlparams['preloadStimuli'];  // F[alse] (default) or T[true]
    if (typeof preloading === 'undefined') {
      preloading = false;
    } else {
      preloading = preloading.toLowerCase();
      if (preloading === 'f' || preloading === 'false') {
        preloading = false;
      } else if (preloading === 't' || preloading === 'true') {
        preloading = true;
      } else throwError('Unrecognized preloadStimuli.');
    }

    // For NORM-D, we developed videos in which the mouth of the speaker is occluded by a box, Starting
    // somewhat prior to the articulation of the s/sh sound (since the preceding vowel can contain co-
    // articulatory information). Our goal in doing so was to reduce the visual label effect (since it
    // can cause ceiling/floor effects that made the interpretation of e.g., Experiment A quite difficult),
    // while maintaining the effect of pen (this is the reason the occluder does show up *after* the
    // start of the video).
    var testOcclusion = e.urlparams['testOcclusion']; // F (default) or T (to use the videos with occlusion)
    if (typeof testOcclusion === 'undefined') {
      testOcclusion = false;
    } else {
      testOcclusion = testOcclusion.toLowerCase();
      if (testOcclusion === 'f' || testOcclusion === 'false') {
        testOcclusion = false;
      } else if (testOcclusion === 't' || testOcclusion === 'true') {
        testOcclusion = true;
      } else throwError('Unrecognized testOcclusion.');
    }
    if (testOcclusion) {
      instruction_occluder = ' with a black box occluding the speaker\'s mouth';
    } else {
      instruction_occluder = '';
    }
    throwMessage('testOcclusion set to: '.concat(testOcclusion));

    // How should X and M be mapped to S and SH responses during exposure?
    var respKeyExp = e.urlparams['respKeyExp'];      // 0 ('X':'word', 'M':'non-word') or 1 ('X':'non-word', 'M':'word')
    // How should X and M be mapped to S and SH responses during test?
    var respKeyTest = e.urlparams['respKeyTest'];    // 0 ('X':'S', 'M':'SH') or 1 ('X':'SH', 'M':'S')
    throwMessage('respKeyExp set to: '.concat(respKeyExp));
    throwMessage('respKeyTest set to: '.concat(respKeyTest));

    var keys_exp, keys_test;
    if (respKeyExp == '0') {
      keys_exp = {'X':'word', 'M':'non-word'};
    } else if (respKeyExp == '1') {
      keys_exp = {'X':'non-word', 'M':'word'};
    } else {
      throwError("Unrecognized response key mapping for exposure.");
    }
    if (respKeyTest == '0') {
      keys_test = {'X':'ASI', 'M':'ASHI'};
    } else if (respKeyTest == '1') {
      keys_test = {'X':'ASHI', 'M':'ASI'};
    } else {
      throwError("Unrecognized response key mapping for test.");
    }

    // How should catch trials be handled during test? (introduced for NORM E in order to figure our whether paying attention to the
    // position of the pen changes the results of NORM D).
    var catch_test = e.urlparams['catch_test']; // F (default) or T (to use the videos with occlusion)
    var catchEndsTrial_test, catchTrialInstruction_test, catchTrialFeedbackTrue_test, instruction_test_catch;
    if (typeof catch_test === 'undefined') {
      catch_test = 'dot';
    } else {
      catch_test = catch_test.toLowerCase();
    }
    if (catch_test === 'dot') {
      catchEndsTrial_test = true;
      catchKey_code_test = 66;
      catchKey_test = String.fromCharCode(catchKey_code_test);
      catchEventDescription_test = "a white dot";
      catchTrialInstruction_test = 'Press "' + catchKey_test + '" when you see a white dot.';
      catchTrialFeedbackTrue_test = 'You saw ' + catchEventDescription_test + ' and pressed "' + catchKey_test + '"!';
      if (label === 'no_exposure') {
        instruction_test_catch = '<strong>As a form of quality control</strong>, you may sometimes be shown a white dot in the video. If you see a white dot, ' +
        'please press "' + catchKey_test + '" instead of answering. Do not press "' + catchKey_test + '" unless you see a white dot. This helps us distinguish ' +
        'you from a robot.<strong>Please note that some participants will never see a white dot</strong>--that is simply chance and nothing to worry ' +
        'about (if there is a white dot, it will be very clear and you won\'t easily miss it). ' +
        'What is most important is that you answer whether the speaker is saying "asi" or a "ashi".</p>';
      } else {
        instruction_test_catch = catchEventDescription_test;
      }
    } else if (catch_test === 'pen') {
      catchEndsTrial_test = false;
      catchKey_code_test = 32;
      catchKey_test = String.fromCharCode(catchKey_code_test);
      catchEventDescription_test = "the pen in the speaker's mouth";
      catchTrialInstruction_test = 'If the pen is in the mouth, first press SPACE. Then answer whether you heard ASI or ASHI.';
      catchTrialFeedbackTrue_test = 'You pressed SPACE. Now also answer whether you heard ASI or ASHI.';
      if (label === 'no_exposure') {
        instruction_test_catch = '<strong>As a form of quality control</strong>, please also indicate whenever you see the pen in the mouth of the speaker. ' +
        'If (and only if) the pen is in the speaker\'s mouth, press SPACE <strong>before</strong> you answer what sound you heard. ' +
        'Then use the other keys to answer what sound you heard. This attention to the video helps us distinguish you from a robot.<br><br>';
      } else {
        instruction_test_catch = catchEventDescription_test;
      }
    } else throwError('Unrecognized catch_test.');
    throwMessage('catch_test set to: '.concat(catch_test));


    ////////////////////////////////////////////////////////////////////////
    // Create and add instructions based on experimental condition (based on whether there is an exposure phase or not)
    ////////////////////////////////////////////////////////////////////////
    var instruction_payment, instruction_experiment, instruction_exposure, instruction_test;
    if (label === 'practice_only') {
      instruction_payment = 'The experiment takes 3-5 minutes to complete and you will be paid $0.40.';
      instruction_experiment = 'You will see and hear a female speaker producing words. Your task is to determine what word the speaker is saying.';
    } else if (label === 'no_exposure') {
      instruction_payment = 'The experiment takes 5-10 minutes to complete and you will be paid $1.00.';
      instruction_experiment = 'You will see and hear a female speaker producing words. Your task is to determine what word the speaker is saying.';
      instruction_test = '<p>You will see and hear videos of a female speaker producing speech' +
                         instruction_occluder +
                         '. <strong>Your task is to decide whether the speaker is saying "asi" or a "ashi".</strong> ' +
                         'Please answer as quickly and accurately as possible, without rushing. You may hear similar sounds several times.<br><br>' +
                         instruction_test_catch;
    } else {
      instruction_payment = 'The experiment takes 15-20 minutes to complete and you will be paid $2.00.';
      instruction_experiment = 'This experiment has two parts. In the first part, you will see and hear a female speaker saying words and non-words. ' +
                         'You will have to determine whether each word she produces is a word or a non-word. In the second part, you will hear words ' +
                         'from the same speaker and determine whether these words contain an "s" or an "sh" sound. The "s" sound is like the sound at ' +
                         'the beginning of the words "sat" and "sofa". The "sh" sound is like the sound at the beginning of the words "shine" and "sheep".';
      // Alternative:
      // instruction_experiment = 'This experiment has two parts. In the first part, you will see and hear a female speaker saying words and non-words. You will have to determine whether each word she produces is a word or a non-word. In the second part, you will see and hear the same speaker and tell us what words the speaker is saying.';
      instruction_exposure = 'That was the end of the practice phase. Now it\'s time to start! <strong>Remember to press the corresponding key on your keyboard to identify ' +
                    'whether the speaker is saying a word of English or not</strong> (press "B" only if you see a white dot, which will be much less often than ' +
                    'during the practice phase).<br><br>' +
                    'Listen and watch carefully, and answer as quickly and accurately as possible.<BR><BR>' +
                    'It is OK to make a few errors---that\'s human! We will only ever reject work when somebody is <em>clearly</em> gaming the ' +
                    'system by pressing random keys, reloading this page, or repeatedly taking this experiment. '
      // Only show this part of the instruction if feedback was given on every trial during practice
      if (practFeedback === true) {
        instruction_exposure = instruction_exposure +
                    'Unlike during practice, you won’t any longer be receiving popup feedback after each trial, but we will still be recording your responses.</p>';
                    } else {
                      instruction_exposure = instruction_exposure + '</p>';
                    }
      instruction_test = '<h3>Phase 2</h3><p>Next, you will see and hear the same speaker as during the preceding parts of the experiment. <strong>' +
                         'This time, your task is to decide whether the speaker is saying "asi" or a "ashi".</strong> Please answer as quickly and ' +
                         'accurately as possible, without rushing. You may hear similar sounds several times.<br><br>As in the preceding parts of ' +
                         'the experiment, press "B" if (and only if) you see ' + instruction_test_catch + '.<br><br>';

      // Only show this part of the instruction if the catch trials are about white dots.
      if (catch_test === 'dot') {
        instruction_test = instruction_test +
                         '<strong>Please note that some participants will never see a white dot in this part of the experiment</strong>---that is ' +
                         'simply chance and nothing to worry about (if there is a white dot, it will be very clear and you won\'t easily miss it). ' +
                         'What is most important is that you answer whether the speaker is saying "asi" or a "ashi".</p>';
      } else {
        instruction_test = instruction_test + '</p>';
      }
    }

    if ($.inArray(skipTo, ['l', 'p', 'e', 't', 's']) < 0) {
      var instructions = new InstructionsSubsectionsBlock(
          {
              logoImg: 'JSEXP/img/logo.png',
              title: 'Listen and click',
              mainInstructions: ['Thank you for your interest in our study!  This is a psychology experiment about how people understand speech. ' +
                                 'You will listen to recorded speech, and press a button on the keyboard to tell us what you heard.',
                                 '<span style="font-weight:bold;">Please read through each of the following requirements. ' +
                                 'If you do not meet all requirements, please do not take this experiment.</span> You can click the names below to expand ' +
                                 'or close each section.'],
              subsections: [
                  {
                      title: 'Experiment length',
                      content: instruction_payment
                  },
                  {
                      title: 'Hardware requirements (mouse + headphones)',
                      content: [{
                        subtitle: 'Mouse',
                        content: 'This experiment requires a mouse (a laptop trackpad will not work).',
                      },
                      {
                        subtitle: 'Headphones',
                        content: "<font color='red'><strong>It is essential that you wear headphones for this experiment.</strong></font> Otherwise your responses may " +
                                 "invalidate our results.<img id='audiopic' src='JSEXP/img/audiotypes.png' width='600'/>"
                      }],
                      checkboxText: 'I am wearing headphones and I am using a mouse.'
                  },
                  {
                      title: 'Language requirements (grew up speaking American English)',
                      content: "You must be a native speaker of American English. " +
                               "<font color='red'><strong>If you have not spent almost all of your time until the age of 10 speaking English and living in the United States, " +
                               "you cannot participate.</strong></font>",
                      checkboxText: 'I am a native American English speaker.'
                  },
                  {
                      title: 'Environment requirements (quiet room)',
                      content: 'Please complete this experiment in one setting and in a quiet room, away from other noise. Please do not look at other web pages or other programs ' +
                               'while completing this experiment. It is important that you give this experiment your full attention.',
                      checkboxText: 'I am in a quiet room and will complete this experiment in one sitting.'
                  },
                  {
                      title: 'Additional requirements',
                      content: ["<font color='red'><strong>Please do not take this experiment multiple times, and do not reload this page.</strong></font> " +
                                'If you are sharing an MTurk/Prolific account with others who have taken this experiment, please make sure that they have not yet taken this experiment. ' +
                                "We cannot use data from reloaded or repeated experiments, and won't be able to approve your work.",
                                "We use cookies and MTurk/Prolific qualifications to make it easy for you to recognize whether you have taken this experiment previously. " +
                                "If you accept our cookies and do not delete them, this should prevent you from accidentally taking the experiment more than once."],
                      checkboxText: 'I (or others with the same worker ID) have not taken this experiment previously.'
                  },
                  {
                      title: 'Sound check',
                      content: ['Please complete the following sound test to make sure your browser is compatible with this experiment, and that your headphones are set to a ' +
                                'comfortable volume. It is important that you keep your speakers at the same volume throughout the experiment and that you do not remove your ' +
                                'headphones after adjusting your sound. Enter the word that you hear into each box.',
                                function() {
                                    var soundcheck = new SoundcheckBlock(
                                        {
                                            items: [
                                                {
                                                    filename: 'JSEXP/sounds/cabbage',
                                                    answer: 'cabbage'
                                                },
                                                {
                                                    filename: 'JSEXP/sounds/lemonade',
                                                    answer: 'lemonade'
                                                }
                                            ],
                                            instructions: ''
                                        }
                                    );
                                    return(soundcheck.init());
                                }]
                  },
                  {
                      title: 'Reasons work can be rejected',
                      content: ['If you pay attention to the instructions and <span style="font-weight:bold;">do not click randomly </span> your work will get approved. ' +
                                '<span style="color:red;"><strong>Please never reload this page, even if you think you made a mistake.</strong></span> ' +
                                'It means that we cannot use your data for scientific purposes. Many, if not most, participants make a few mistakes in our experiments. ' +
                                "That's perfectly OK and might, in fact, be of interest to us. We reject far less than 1% of all completed experiments.",
                                'We will only reject work if you a) <strong>clearly</strong> do not pay attention to the instructions, b) reload the page, or c) repeat ' +
                                'the experiment.'],
                      checkboxText: 'I understand the reasons my work might get rejected.'
                  },
                  {
                      title: 'Experiment instructions',
                      content: instruction_experiment,
                      checkboxText: 'I have read and understood the instructions.'
                  },
                  {
                      title: 'Informed consent',
                      content: e.consentFormDiv,
                      checkboxText: 'I consent to participating in this experiment'
                  },
                  {
                      title: 'Further (optional) information',
                      content: ['Sometimes it can happen that technical difficulties cause experimental scripts to freeze so that you will not be able to submit this experiment. ' +
                                'We are trying our best to avoid these problems. Should they nevertheless occur, we urge you to (1) take a screen shot of your browswer ' +
                                'window, (2) if you know how to also take a screen shot of your Javascript console, and (3) ' +
                                '<a href="mailto:hlplab@gmail.com">email us</a> this information along with the HIT/Experiment ID and your worker/Prolific ID. ',
                                'If you are interested in hearing how the experiments you are participating in help us to understand the human brain, feel free to ' +
                                'subscribe to our <a href="http://hlplab.wordpress.com/">lab blog</a> where we announce new findings. Note that typically about 1-2 years ' +
                                'pass before an experiment is published.'],
                      finallyInfo: true
                  }
              ]
          }
      );
      e.addBlock({
          block: instructions,
          onPreview: true});
    }


    // Prevent loading the videos on preview.
    if (e.previewMode) {
      e.nextBlock();
    } else {
      ////////////////////////////////////////////////////////////////////////
      // Function to collect all stimulus filenames (for preloading)
      ////////////////////////////////////////////////////////////////////////
      var all_stimulus_filenames = [];
      var add_stimulus_filenames = function(stimulus_filenames, new_stimuli) {
        var new_stimulus_filenames = [];
        for (var i = 0; i < new_stimuli.filenames.length; i++) { new_stimulus_filenames[i] = new_stimuli.prefix + new_stimuli.filenames[i]; }
        stimulus_filenames = stimulus_filenames.concat(new_stimulus_filenames);

        // throwMessage('Updated list of stimulus filenames: ' + stimulus_filenames);
        return(stimulus_filenames);
      }

      if (label !== 'no_exposure') {
        ////////////////////////////////////////////////////////////////////////
        // Create PRACTICE block
        //
        // We restructured the practice phase for Experiment B to provide feedback and to enforce perfection.
        // We also changed from 2 to 4 practice trials that cover all possible combinations of catch vs. no
        // catch trial and word vs. non-word. This an other small changes in the instructions seem to have the
        // unintentional effect of shifting participants' attention to the visual aspects of the stimuli, rather
        // than the auditory input. The PR effect was no longer significant in Experiment B.
        //
        // For Experiment C, we therefore revised the feedback to be more uniform and to emphasize the importance
        // of the lexical decision task. We changed the instructions with the same goal in mind: de-emphasizing
        // the importance of the video and emphasizing the importance of the audio. We further changed the practice
        // trials by including two more trials.
        ////////////////////////////////////////////////////////////////////////
        var stimuli_practice = new StimuliFileList({
            prefix: 'stimuli/practice/',
            mediaType: 'video',
            filenames: ['F40H', 'FN86H-CATCH', 'FN6M', 'F2H-CATCH', 'F4M', 'FN1H'],
            // Hash that maps filenames to expected response. required if the block uses provideFeedback = true
            mappingStimulusToCorrectResponse: {
              'F40H' : 'word',
              'FN86H-CATCH' : 'non-word',
              'FN6M' : 'non-word',
              'F2H-CATCH' : 'word',
              'F4M' : 'word',
              'FN1H' : 'non-word' }
        });
        var block_practice = new IdentificationBlock({
            stimuli: stimuli_practice,
            respKeys: keys_exp,
            provideFeedback: practFeedback, // if true, provides feedback about correct (expected) response when participants make mistakes
            enforcePerfection: practEnforcePerfection, // if true, forces reset (repeat) of block every time a mistake is made.
            stimOrderMethod: "shuffle_across_blocks",
            namespace: 'practice'
        });
        all_stimulus_filenames = add_stimulus_filenames(all_stimulus_filenames, stimuli_practice);

        if (label !== 'practice_only') {
          ////////////////////////////////////////////////////////////////////////
          // Create EXPOSURE block
          ////////////////////////////////////////////////////////////////////////
          var filenames_exposure = get_pseudorandomized_exposure_list(label, condition, expSet);
          // If expSet == C make sure to also add filenames for second (i.e. typical) part of experiment
          if (expSet === 'C') {
            filenames_exposure = filenames_exposure.concat(get_pseudorandomized_exposure_list('all_typical', 'H', 'C-typical'));
          }
          throwMessage("Exposure list: " + filenames_exposure);

          var stimuli_exposure = new StimuliFileList({
            prefix: 'stimuli/exposure/',
            mediaType: 'video',
            // Switch this if we want fixed pseduo-random order rather than pseudo-randomization on the fly
            // filenames: get_exposure_list(list_num, label, condition, exp_reverse)
            filenames: filenames_exposure
          });

          var block_exposure = new IdentificationBlock({
            stimuli: stimuli_exposure,
            respKeys: keys_exp,
            stimOrderMethod: "dont_randomize",
            namespace: 'exposure'
          });
          all_stimulus_filenames = add_stimulus_filenames(all_stimulus_filenames, stimuli_exposure);
        }
      }

      if (label !== 'practice_only') {
        ////////////////////////////////////////////////////////////////////////
        // Create TEST block
        ////////////////////////////////////////////////////////////////////////
        // Video test block with six repetitions of 12 videos (6 sounds from the s-sh continuum x with video that either
        // shows the pen in the mouth or pen in the hand) in randomized order. Note that the instructions are slightly
        // changed compared to Liu and Jaeger (2018) in order to a) accommodate the fact that test trials now have video,
        // b) highlight the fact that the exposure and test talker are the same, and c) talk about asi and ashi rather than
        // s and sh. The old instructions can be found in the Liu and Jaeger (2018) experiment.
        var stimuli_test = new StimuliFileList({
          prefix: 'stimuli/test/',
          mediaType: 'video',
          filenames: get_pseudorandomized_test_list(testSet, testOcclusion),
        });
        var block_test = new IdentificationBlock({
          stimuli: stimuli_test,
          respKeys: keys_test,
          stimOrderMethod: "dont_randomize",
          // categories: ['S','SH'], // commented out so that categories are inferred from key mapping (keys_test)
          namespace: 'test',
          catchEndsTrial: catchEndsTrial_test,
          catchKeyCode: catchKey_code_test,
          catchEventDescription: catchEventDescription_test,
          catchTrialInstruction: catchTrialInstruction_test,
          catchTrialFeedbackTrue: catchTrialFeedbackTrue_test
        });
        all_stimulus_filenames = add_stimulus_filenames(all_stimulus_filenames, stimuli_test);
      }


      ////////////////////////////////////////////////////////////////////////
      // Add PRELOADING block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['p', 'e', 't', 's']) < 0 && preloading) {
        throwMessage("Preparing preloading block.");
        // Get all the unique filenames
        var unique_stimulus_filenames = all_stimulus_filenames.filter(function(item, pos, self) { return self.indexOf(item) == pos; });
        throwMessage('Preparing list of unique stimulus files for preloading: ' + unique_stimulus_filenames);

        var block_preloading = new MediaLoadingBlock({
          stimuli: new ExtendedStimuliFileList({
            prefix: '',
            mediaType: 'video',
            filenames:   unique_stimulus_filenames,
            subtitles:   Array.apply(null, Array(unique_stimulus_filenames.length)).map(function(){return ""})
          }),
          totalLoadingThreshold: -1, // For 1 minute: 60000
          namespace: 'preload'
        });

        throwMessage("Adding preload block.");
        e.addBlock({
          block: block_preloading,
          instructions: "<p>Before you begin the experiment, " +
          "we need to pre-load the video files now so they don't cause interruptions " +
          "during the rest of the experiment.</p>" +
          '<p>This will also give you an idea of your connection speed to our server. ' +
          'If for some reason the files are loading very slowly, please move on and don\'t take this experiment. ' +
          'We apologize for the inconvenience.</p>',
          onPreview: false
        });
      }

      ////////////////////////////////////////////////////////////////////////
      // Add PRACTICE block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['e', 't', 's']) < 0 && label !== 'no_exposure') {
        throwMessage("Adding practice block.");

        var instructions_practice = '<h3>Phase 1</h3><p>You will watch videos of a speaker. For each video, <strong>your task is to identify whether the speaker is producing ' +
                     'a word of English or not.</strong> Do this task carefully, but also as quickly and as accurately as possible.<br><br>' +
                     'As a form of quality control, you may sometimes see a white dot in the video. If it occurs, it is easy to see. ' +
                     'If you see a white dot, please press "B" instead of answering. Do not press "B" unless you see a white dot. This helps us ' +
                     'distinguish you from a robot. Most important, however, is that you listen carefully and tell us whether you hear a word of English or not.';
        if (label !== 'practice_only') instructions_practice += '<br><br>Here are some practice trials with feedback to familiarize you with the task.</p>';

        e.addBlock({
            block: block_practice,
            instructions: instructions_practice,
            onPreview: false
        });
      }

      ////////////////////////////////////////////////////////////////////////
      // Add EXPOSURE block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['t', 's']) < 0 && label !== 'no_exposure' && label !== 'practice_only') {
        throwMessage("Adding exposure block.");
        e.addBlock({
            block: block_exposure,
            instructions: instruction_exposure,
            onPreview: false
        });
      }

      ////////////////////////////////////////////////////////////////////////
      // Add TEST block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['s']) < 0 && label !== 'practice_only') {
        throwMessage("Adding test block.");
        e.addBlock({
            block: block_test,
            instructions: instruction_test,
            onPreview: false
        });
      }

      $("#continue").hide();
      e.nextBlock();
    }
});
