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
 * Modified by:Florian Jaeger (2020)
 *
 *    The code was further modified for the replication of Wade et al (2007)
 */

 var RESP_DELIM = ';';
 // Experiment object to control everything
 var e;


$(document).ready(function() {
  ////////////////////////////////////////////////////////////////////////
  // General setup
  ////////////////////////////////////////////////////////////////////////
  // take break every k trials
  var breakEvery = 50;

  ////////////////////////////////////////////////////////////////////////
  // Create experiment
  ////////////////////////////////////////////////////////////////////////
  e = new Experiment({
      rsrbProtocolNumber: 'RSRB00045955',
      rsrbConsentFormURL: 'https://www.hlp.rochester.edu/mturk/consent/RSRB45955_Consent_2022-02-04.pdf',
      survey: 'surveys/post_survey.html',
      cookie: 'DLVOT' // vowel perception L2
  });
  e.init();

  ////////////////////////////////////////////////////////////////////////
  // Parse relevant URL parameters -- USER DEFINED
  ////////////////////////////////////////////////////////////////////////

  // Use the following parameter to skip parts of the experiment for debugging:
  var skipTo = e.urlparams['skipTo'];             // pre-[l]oading, p[ractice], e[xposure], t[est], s[urvey]
  var cond_exp = e.urlparams['cond_exp'];         // no_exposure, lvar, or hvar
  if ($.inArray(cond_exp, ['no_exposure', 'lvar', 'hvar']) < 0) throwError('unrecognized cond_exp.');
  var list_exp = e.urlparams['list_exp'];       // name of test list (without path or file extension)
  if (typeof(list_exp) === 'undefined') list_exp = 'builtin';
  if ($.inArray(list_exp, ['builtin']) < 0) throwError('unrecognized list_exp.');
  // var cond_test = e.urlparams['cond_test'];       // lvar or hvar
  // if ($.inArray(cond_test, ['lvar', 'hvar']) < 0) throwError('unrecognized cond_test.');
  var list_test = e.urlparams['list_test'];       // name of test list (without path or file extension)
  if (typeof(list_test) === 'undefined') list_test = 'builtin';
  if ($.inArray(list_test, ['builtin', 'NORM-A-forward-test', 'NORM-A-backward-test']) < 0) throwError('unrecognized list_test.');

  // set instructions based on experimental condition (based on whether there is an exposure phase or not)
  var instruction_payment, instruction_experiment, instruction_test;
  if (cond_exp === 'no_exposure') {
    experiment_duration = 15;
    experiment_payment = '$1.50';
    instruction_experiment = 'In this experiment, you will hear a female speaker saying words. Your task is to determine which word the speaker is saying by using ' +
                             'your mouse to click on the correct answer.';
    instruction_test = "<p>Now let's begin the experiment.<p>" +
                       "<p>Your task is to decide which of two words displayed the speaker said by clicking on it. <strong>Listen carefully, and answer as quickly " +
                       "and accurately as possible.</strong> You might feel that recordings are repeated as many of the recordings differ only in rather subtle ways.</p>"
  } else {
    experiment_duration = 100000;
    experiment_payment = '$XXX.XX';
    instruction_experiment = 'In this experiment, you will hear a female speaker saying words. Your task is to determine what word the speaker is saying by using your mouse to click on the correct answer. The experiment has two parts. In Part 1, you will learn how the speaker pronounces her words. In Part 2, we will assess how your perception is affected by what you have learned in Part 1.';
    instruction_exposure = '<h3>Learning phase</h3><p>Your task is to decide which of the following words the speaker is saying: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. <strong>Listen carefully, and answer as quickly and accurately as possible.</strong><br><br>' +
    '<strong>You will receive feedback after each response you provide:</strong> all but the correct word will be removed from the screen, and you will hear the recording of this word again. If you answered incorrectly, the correct word will blink for a few seconds before the recording will be played again.<br><br>' +
    'This phase of the experiment will continue until you have answered 30 out of any 50 consecutive words correctly. <strong>This is a difficult learning task and might take some time</strong> (most participants need a few hundred trials). We appreciate your patience and attention, which helps us learn about how the brain understands speech.';
    instruction_test = '<h3>Test phase</h3><p>Next, you will hear the same speaker as during the first phase of the experiment. Your task is the same, to decide what word the speaker is saying. This time, however, <strong>you will not get feedback</strong> on your answer.<br><br>' +
    '<strong>Listen carefully, and answer as quickly and accurately as possible.</strong> As during the first part of the experiment, you might feel that recordings are repeated, as many of the recordings differ only in rather subtle ways.</p>';
  }
  instruction_payment = 'The experiment will take ' + experiment_duration + ' minutes to complete and you will be paid ' + experiment_payment + '.';

  ////////////////////////////////////////////////////////////////////////
  // Create and add instructions
  ////////////////////////////////////////////////////////////////////////
  if ($.inArray(skipTo, ['l', 'p', 'e', 't', 's']) < 0) {
    var instructions = new InstructionsSubsectionsBlock(
        {
            logoImg: 'JSEXP/img/logo.png',
            title: 'Listen and click',
            mainInstructions: ['Thank you for your interest in our study!  This HIT is a psychology experiment about how people understand speech. ' +
                               'You will listen to recorded speech, and press a button on the keyboard to tell us what you heard.',
                               '<span style="font-weight:bold;">Before you accept this HIT, please read through each of the following requirements. ' +
                               'If you do not meet all requirements, please do not accept this HIT.</span> You can click the names below to expand ' +
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
                    content: 'Please complete this HIT in one setting and in a quiet room, away from other noise. Please do not look at other web pages or other programs ' +
                             'while completing this HIT. It is important that you give this experiment your full attention.',
                    checkboxText: 'I am in a quiet room and will complete this HIT in one sitting.'
                },
                {
                    title: 'Additional requirements',
                    content: ["<font color='red'><strong>Please do not take multiple HITs of this experiment and do not reload this page once you have accepted the HIT.</strong></font> " +
                              'If you are sharing an account with other MTurkers who have taken this experiment, please make sure that they have not yet taken this experiment. ' +
                              "We cannot use data from reloaded or repeated HITs, and won't be able to approve your work.",
                              "We use cookies and MTurk qualifications to make it easy for you to recognize whether you have taken this experiment previously. " +
                              "If you accept our cookies and do not delete them, this should prevent you from accidentally taking the experiment more than once."],
                    checkboxText: 'I (or others with the same worker ID) have not taken this HIT previously.'
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
                              "That's perfectly OK and might, in fact, be of interest to us. We reject far less than 1% of all completed HITs.",
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
                    content: ['Sometimes it can happen that technical difficulties cause experimental scripts to freeze so that you will not be able to submit a HIT. ' +
                              'We are trying our best to avoid these problems. Should they nevertheless occur, we urge you to (1) take a screen shot of your browswer ' +
                              'window, (2) if you know how to also take a screen shot of your Javascript console, and (3) ' +
                              '<a href="mailto:hlplab@gmail.com">email us</a> this information along with the HIT ID and your worker ID. ',
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
  } // end of instruction block

  ////////////////////////////////////////////////////////////////////////
  // Function that adds all the blocks when everything's ready and runs the experiment
  // This function is run every time a papa parse completes. Just add all the stimuli together
  // until the final block is reached. Then start creating and adding blocks.
  ////////////////////////////////////////////////////////////////////////
  // declared here so that papaparse can fill these vars and continue_experiment can read them
  var practiceStimuli, exposureStimuli, testStimuli;
  var all_audio_filenames = [];
  var continue_experiment = function(block, stimuli) {
    // Add stimuli to those that need to be preloaded and add path prefix to all filenames
    var filenames = [];
    for (var i = 0; i < stimuli.filenames.length; i++) { filenames[i] = stimuli.prefix + stimuli.filenames[i]; }
    all_audio_filenames = all_audio_filenames.concat(filenames);
    throwMessage('Updated list of all stimuli: ' + all_audio_filenames);

    if (block === 'test') {
      ////////////////////////////////////////////////////////////////////////
      // Create and add PRELOADING block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['p', 'e', 't', 's']) < 0) {
        throwMessage("Preparing preloading block.");
        // Get all the unique filenames
        var unique_audio_filenames = all_audio_filenames.filter(function(item, pos, self) { return self.indexOf(item) == pos; });
        throwMessage('Preparing list of unique audio files for preloading: ' + unique_audio_filenames);

        var preloadingBlock = new MediaLoadingBlock({
          stimuli: new ExtendedStimuliFileList({
            prefix: '',
            mediaType: 'audio',
            filenames:   unique_audio_filenames,
            subtitles:   Array.apply(null, Array(unique_audio_filenames.length)).map(function(){return ""})
          }),
          totalLoadingThreshold: -1, // For 1 minute: 60000
          namespace: 'preload'
        });

        e.addBlock({
          block: preloadingBlock,
          instructions: "<p>Before you begin the experiment, " +
          "we need to pre-load the audio files now so they don't cause interruptions " +
          "during the rest of the experiment.</p>" +
          '<p>This will also give you an idea of your connection speed to our server. ' +
          'If for some reason the files are loading very slowly, you can return this HIT and move on, ' +
          'without wasting your time on the rest of the experiment.</p>',
          onPreview: false
        });
      } // end of preloading block

      ////////////////////////////////////////////////////////////////////////
      // Create and add PRACTICE block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['e', 't', 's']) < 0 && cond_exp !== 'no_exposure') {
        throwMessage("Preparing practice block.");
        throwMessage("... practice block not yet implemented");
      } // end of practice block

      ////////////////////////////////////////////////////////////////////////
      // Create and add EXPOSURE block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['t', 's']) < 0 && cond_exp !== 'no_exposure') {
        throwMessage("Preparing exposure block.");

        var exposureBlock = new VisualGridBlock({
          stimuli: exposureStimuli,
          images: stimImages,
          instructions: instruction_exposure,
          imageMapping: imageMapping,
          namespace: 'exposure',
          allowFeedback: true,
          autoAdvanceReady: true,
          ITI_trialStartToImages: 500,  // time from trial start to showing pictures
          ITI_imagesToAudioStart: 1000,  // time from trial to start to audio play (only relevant if autoAdvanceReady == T)
          ITI_responseToTrialEnd: 1500,
          OnNegativeFeedback_blinkInterval: 200, // how long is the blink on and off?
          OnNegativeFeedback_blinkNumber: 4,     // how many blinks are shown? (takes blinkInterval ms per blink)
          breakEvery: breakEvery,  // Take a break every x trials
          imagePositions: [
            'topleft', 'topcenter', 'topright',
            'midleft', 'midright',
            'bottomleft', 'bottomcenter', 'bottomright'],
            randomizeImagePositions: false, // Is true by default. If false, then just uses the list order above
            randomizationMethod: 'shuffle_blocks', // Can also be set to "dont_randomize"
            showFamiliarization: false,
            debugMode: e.debugMode
        });

        e.addBlock({
          block: exposureBlock,
          onPreview: false,
          showInTest: true
        });
      } // end of exposure block

      ////////////////////////////////////////////////////////////////////////
      // Create and add TEST block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['s']) < 0) {
        /* Create visual world block using the stimuli above */
        var testBlock = new VisualGridBlock({
          stimuli: testStimuli,
          images: stimImages,
          instructions: instruction_test,
          imageMapping: imageMapping,
          namespace: 'test',
          allowFeedback: false,
          autoAdvanceReady: true,
          ITI_trialStartToImages: 500,  // time from trial start to showing pictures
          ITI_imagesToAudioStart: 1000,  // time from trial to start to audio play (only relevant if autoAdvanceReady == T)
          ITI_responseToTrialEnd: 0,
          breakEvery: breakEvery,  // Take a break every x trials
          imagePositions: [
            'topleft', 'topright'],
            randomizeImagePositions: false, // Is true by default. If false, then just uses the list order above
            randomizationMethod: 'shuffle_blocks', // Can also be set to "dont_randomize"
            showFamiliarization: false,
            debugMode: e.debugMode
        });

        e.addBlock({
          block: testBlock,
          onPreview: false,
          showInTest: true
        });
      } // end of test block



      // All blocks have been added
      $("#continue").hide();
      e.nextBlock();
    } // end of if (block === 'test')
  } // end of continue_experiment() function

  // Prevent loading of stimuli on preview.
  if (e.previewMode) {
    e.nextBlock();
  } else {
    // For ending the experiment when the participant fails
    //  var myEndedHandler = function() {
    //          // Adds the cookie here, repeatedly, sure, but w/e
    //          createCookie('tc1',1,14);
    //          var didwork = readCookie('tc1')
    //          if (didwork == null) {
    //                  alert('You need to have cookies enabled for this experiment!');
    //          }
    //      if (bad_survey_response_bool || failed_dprime || too_slow_loading || (!wants_to_continue)) {
    //          failed_out = true;
    //      }
    //      // If they fail and it's past the survey block
    //      if (failed_out && e.blockn >= fail_after_n) {
    // //                 alert('You failed out!');
    // //                 alert("Failure vars: "+"bad_survey."+bad_survey_response_bool + RESP_DELIM +
    // //                          "failed_dprime."+failed_dprime         + RESP_DELIM +
    // //                          "too_slow."+too_slow_loading           + RESP_DELIM +
    // //                          "wants_to_stop."+(!wants_to_continue));
    //          e.wrapup("bad_survey."+bad_survey_response_bool + RESP_DELIM +
    //                   "failed_dprime."+failed_dprime         + RESP_DELIM +
    //                   "too_slow."+too_slow_loading           + RESP_DELIM +
    //                   "wants_to_stop."+(!wants_to_continue)
    //                   );
    //      } else {
    //          e.nextBlock();
    //      }
    //  };

    ////////////////////////////////////////////////////////////////////////
    // Define general materials and mappings for visual grids
    ////////////////////////////////////////////////////////////////////////
    /* Define the images corresponding to each spoken sound/vowel/word */
    var stimImages = {
        dill: 'stimuli/images/dill_image.png',
        till: 'stimuli/images/till_image.png',
        dim: 'stimuli/images/dim_image.png',
        tim: 'stimuli/images/tim_image.png',
        din: 'stimuli/images/din_image.png',
        tin: 'stimuli/images/tin_image.png',
        dip: 'stimuli/images/dip_image.png',
        tip: 'stimuli/images/tip_image.png',
        other: 'stimuli/images/other_image.png'
    };

    // Define which images will show for a given word depending on the the condition (forward/backward) of that trial
    //
    //    'condition_name': {
    //           'word_1': ['img_1', .., 'img_k'],
    //           ..
    //           'word_j': ['img_1', .., 'img_k'],
    //     }
    var imageMapping = {
      'forward': {
        'TEST': ['dill', 'till', 'dim', 'tim', 'din', 'tin', 'dip', 'tip'],
        'TEST.dilltill': ['dill', 'till'],
        'TEST.dimtim': ['dim', 'tim'],
        'TEST.dintin': ['din', 'tin'],
        'TEST.diptip': ['dip', 'tip'],
        'dill': ['dill', 'till'],
        'till': ['dill', 'till'],
        'dim': ['dim', 'tim'],
        'tim': ['dim', 'tim'],
        'din': ['din', 'tin'],
        'tin': ['din', 'tin'],
        'dip': ['dip', 'tip'],
        'tip': ['dip', 'tip']},
      'backward': {
        'TEST': ['dill', 'till', 'dim', 'tim', 'din', 'tin', 'dip', 'tip'].reverse(),
        'TEST.dilltill': ['dill', 'till'].reverse(),
        'TEST.dimtim': ['dim', 'tim'].reverse(),
        'TEST.dintin': ['din', 'tin'].reverse(),
        'TEST.diptip': ['dip', 'tip'].reverse(),
        'dill': ['dill', 'till'].reverse(),
        'till': ['dill', 'till'].reverse(),
        'dim': ['dim', 'tim'].reverse(),
        'tim': ['dim', 'tim'].reverse(),
        'din': ['din', 'tin'].reverse(),
        'tin': ['din', 'tin'].reverse(),
        'dip': ['dip', 'tip'].reverse(),
        'tip': ['dip', 'tip'].reverse()}
    };

    ////////////////////////////////////////////////////////////////////////
    // Create and add EXPOSURE stimuli
    ////////////////////////////////////////////////////////////////////////
    throwMessage("Preparing exposure block.");
    var exposureStimuli;

    if (list_exp === 'builtin') {
      // manually defined stimulus list
      var exposureStimuli = new ExtendedStimuliFileList({
        prefix: 'stimuli/exposure/',
        filenames: [

        ],
        target_words: [

        ],
        image_selections: ['forward', 'forward', 'forward', 'forward', 'forward', 'forward', 'forward', 'forward'],
        feedback: ['true', 'true', 'true', 'true', 'true', 'true', 'true', 'true'],
        reps: [1, 1, 1, 1, 1, 1, 1, 1]
      });
    } else {
      var exposureList = 'lists/exposure_sample_' + cond_exp + '.csv';

      Papa.parse(exposureList, {
        download: true,
        header: true,
        delimiter: ',',
        skipEmptyLines: true,
        complete: function(list) {
          exposureStimuli = new ExtendedStimuliFileList({
            prefix: 'stimuli/exposure/',
            filenames: getFromPapa(list, 'filename'),
            target_words: getFromPapa(list, 'target_word'),
            image_selections: getFromPapa(list, 'image_selection'),
            feedback: getFromPapa(list, 'feedback'),
            reps: getFromPapa(list, 'reps')
          });
          throwMessage('Parsed exposure list.');
          continue_experiment('exposure', exposureStimuli);
        }
      });
    }

    ////////////////////////////////////////////////////////////////////////
    // Create and add TEST stimuli
    ////////////////////////////////////////////////////////////////////////
    throwMessage("Preparing test block.");
    var testStimuli;

    if (list_test === 'builtin') {
      testStimuli = new ExtendedStimuliFileList({
        prefix: 'stimuli/test/',
        filenames: [
         'dill_VOT0_PV0_F0246.wav', 'dill_VOT40_PV0_F0248.wav', 'dim_VOT0_PV50_F0244.wav', 'dim_VOT30_PV0_F0247.wav',
         'din_VOT60_PV0_F0248.wav', 'din_VOT10_PV0_F0246.wav', 'dip_VOT0_PV100_F0242.wav','dip_VOT85_PV0_F0249.wav'
        ],
        target_words: [
          'dill', 'dill',
          'dim', 'dim',
          'din', 'din',
          'dip', 'dip'
        ],
        image_selections: ['forward', 'forward', 'forward', 'forward', 'forward', 'forward', 'forward', 'forward'],
        feedback: ['false', 'false', 'false', 'false', 'false', 'false', 'false', 'false'],
        reps: [1, 1, 1, 1, 1, 1, 1, 1]
      });
      continue_experiment('test', testStimuli);
    } else {
      var testList = 'lists/' + list_test + '.csv';

      throwMessage("Parsing test list.");
      Papa.parse(testList, {
        download: true,
        header: true,
        delimiter: ',',
        skipEmptyLines: true,
        complete: function(list) {
          testStimuli = new ExtendedStimuliFileList({
            prefix: 'stimuli/test/norm-list/',
            filenames: getFromPapa(list, 'filename'),
            target_words: getFromPapa(list, 'target_word'),
            image_selections: getFromPapa(list, 'image_selection'),
            feedback: getFromPapa(list, 'feedback'),
            reps: getFromPapa(list, 'reps')
          });
          throwMessage('Parsed test list.');
          continue_experiment('test', testStimuli);
        }
      });
      throwMessage("Done parsing test list.");
    }
  } // end of everything that is only shown when not in preview mode
}); // end of document ready function
