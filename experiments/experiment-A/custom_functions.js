// adapted from https://www.developphp.com/video/JavaScript/Get-or-Remove-Random-Array-Elements-Tutorial
Array.prototype.randsplice = function(n) {
  var ri, rs = [];
  for (i = 0; i < n; i++) {
    ri = Math.floor(Math.random() * this.length);
    rs.push(this.splice(ri, 1));
  }
  rs = rs.flat(1);
  return rs;
}

// this function was used for experiment A and NORM A, B
function get_pseudorandomized_exposure_list(label, condition, expSet) {
  var numCriticalPerLabel = 10;
  var itemID_Critical, filenames_FillerWord, filenames_FillerNonWord, filenames_Catch, block_length;


  if (expSet === 'A') {
    block_length = 10;
    itemID_Critical = ["1", "3", "5", "7", "9", "11", "13", "15", "17", "19"];

    // Added F57M to F from F-CATCH compared LJ18
    filenames_FillerWord = ['F3M','F5M','F7H',
                            'F11H','F13M','F15H','F17H','F19H',
                            'F21H','F23M','F25M','F27H','F28M',
                            'F33H','F35M',
                            'F41H','F43M','F45M','F47H','F49H',
                            'F50M','F53H','F55M','F57M','F59M'];
    // Added FN35H to FN from FN-CATCH compared LJ18
    filenames_FillerNonWord = ['FN1M','FN5H','FN7M','FN9M',
                                'FN11M','FN13H','FN15M','FN17M','FN19H',
                                'FN21H','FN23H','FN25H','FN27M','FN29M',
                                'FN31M','FN33H','FN35H','FN37M','FN39M',
                                'FN41H','FN43M','FN45M','FN47M','FN49H',
                                'FN55M','FN53M','FN57H','FN59H',
                                'FN61H','FN63M','FN65H','FN69H',
                                'FN71H','FN73M','FN75H','FN77H',
                                'FN81H','FN83M','FN85H','FN89M',
                                'FN90H','FN93H','FN95H','FN97M','FN99M'];
    /* 5 instead of 6 CATCH trials for F and FN each compared to LJ18
       This change was made in order to make counter balancing easier.
       Half of the CATCH trials were on filler words, half on filler non-words.
       Half of the CATCH trials were on pen-in-hand, half on pen-in-mouth trials. */
    filenames_Catch = ['F1M-CATCH','F9H-CATCH','FN51H-CATCH','F31H-CATCH','F39M-CATCH',
                       'FN3M-CATCH','F37H-CATCH','FN67M-CATCH','FN79H-CATCH','FN87M-CATCH'];
  } else if (expSet === 'B') {
    // Starting in Exp B, we replaced some items because of lexical decision accuracy (see write-up)
    // Subsituted item 10 for item 9
    block_length = 10;
    itemID_Critical = ["1", "3", "5", "7", "10", "11", "13", "15", "17", "19"];

    // Same as expSet A.
    filenames_FillerWord = ['F3M','F5M','F7H',
                            'F11H','F13M','F15H','F17H','F19H',
                            'F21H','F23M','F25M','F27H','F28M',
                            'F33H','F35M',
                            'F41H','F43M','F45M','F47H','F49H',
                            'F50M','F53H','F55M','F57M','F59M'];
    /* Substituted non-words that were similar to words with better non-words
       Changed FN15M to FN24M
	     Changed FN31M to FN48M
	     Changed FN59H to FN66H
	     Changed FN97M to FN46M */
    filenames_FillerNonWord = ['FN1M','FN5H','FN7M','FN9M',
                                'FN11M','FN13H','FN24M','FN17M','FN19H',
                                'FN21H','FN23H','FN25H','FN27M','FN29M',
                                'FN48M','FN33H','FN35H','FN37M','FN39M',
                                'FN41H','FN43M','FN45M','FN47M','FN49H',
                                'FN55M','FN53M','FN57H','FN66H',
                                'FN61H','FN63M','FN65H','FN69H',
                                'FN71H','FN73M','FN75H','FN77H',
                                'FN81H','FN83M','FN85H','FN89M',
                                'FN90H','FN93H','FN95H','FN46M','FN99M'];
    /* Same as for expSet A. */
    filenames_Catch = ['F1M-CATCH','F9H-CATCH','FN51H-CATCH','F31H-CATCH','F39M-CATCH',
                       'FN3M-CATCH','F37H-CATCH','FN67M-CATCH','FN79H-CATCH','FN87M-CATCH'];
  } else if (expSet === 'C') {
    /*         For Exp XXX, we reduced the number of non-word fillers to 30 (from 50) and the number of
       fillers to 10 (from 30). The fillers are thus a subset of those in expSet B. expSet C is
       the first half of Exp XXX (the shifted stimuli).
               Intended effect is a catch trial every ~10 trials, as in expSet A & B. Current code adds 1 CATCH per block of 6, and therefore uses up the 6 alloted
       CATCHES before the last 4 of 10 total blocks. To combat this, 2 Filler words and 2 Filler nonwords (crucially, NOT catches) are added to the
       filenames_Catch array. This has the effect of including catches in 6 of 10 blocks (of 6 trials), with the remaining 4 including instead either a normal FW or FNW. */
    block_length = 6;
    itemID_Critical = ["1", "3", "5", "7", "10", "11", "13", "15", "17", "19"];

    // 7 filler word (+ 3 filler word catch trials). 3 with pen in hand (H), and 4 with pen in mouth (m).  One of each of these is moved to the filenames_Catch array.
    filenames_FillerWord = ['F7H','F11H',
                            'F3M','F5M','F13M'];
    // 27 filler non-words (+ filler non-word catch trials). 14 with pen in hand (H), and 13 with pen in mouth (m).  One of each of these is moved to the filenames_Catch array.
    filenames_FillerNonWord = ['FN5H','FN13H','FN19H','FN21H','FN23H',
                               'FN25H','FN33H','FN35H','FN41H','FN49H',
                               'FN57H','FN66H','FN61H',
                               'FN1M','FN7M','FN9M','FN11M','FN24M',
                               'FN17M','FN27M','FN29M','FN48M','FN37M',
                               'FN39M','FN43M'];

    /* To have the same *ratio* of catch trials, we use only 6 (instead of 10). As in the other
       expSets, half of these are FN, half F; half are M, half are H. */
    filenames_Catch = ['F1M-CATCH','F9H-CATCH', 'F31H-CATCH',
                       'FN3M-CATCH', 'FN67M-CATCH','FN79H-CATCH',
                       'F15H', 'F23M', //taken from filenames_FillerWord array
                       'FN65H','FN45M']; //taken from filenames_FillerNonWord array

  } else if (expSet === 'C-typical') {
    /* expSet D is the second half of Exp XXX (the typical/non-shifted stimuli). */
    block_length = 6;
    itemID_Critical = ["2", "4", "6", "8", "10", "12", "14", "16", "18", "20"];

    // Here, we follow the same treatment as above with regard to catch trials.
    // 7 filler word (+ 3 filler word catch trials). 4 with pen in hand (H), and 3 with pen in mouth (m). One of each of these is moved to the filenames_Catch array.
    filenames_FillerWord = ['F17H','F19H','F21H',
                            'F25M','F28M'];
    // 27 filler non-words (+ filler non-word catch trials). 13 with pen in hand (H), and 14 with pen in mouth (m).  One of each of these is moved to the filenames_Catch array.
    filenames_FillerNonWord = ['FN69H','FN71H','FN75H','FN77H','FN81H',
                               'FN85H','FN90H','FN93H','FN95H', 'FN2H',
                               'FN4H', 'FN8H',
                               'FN47M','FN55M','FN53M','FN63M','FN73M',
                               'FN83M','FN89M','FN46M','FN99M','FN14M',
                               'FN16M', 'FN18M', 'FN20M'];

    /* To have the same *ratio* of catch trials, we use only 6 (instead of 10). As in the other
       expSets, half of these are FN, half F; half are M, half are H. */
    filenames_Catch = ['F37H-CATCH','F39M-CATCH',
                       'FN51H-CATCH','FN87M-CATCH',
                       'F6M-CATCH','FN12H-CATCH',
                       'F27H', 'F35M', //taken from filenames_FillerWord array
                       'FN10H','FN22M']; //taken from filenames_FillerNonWord array
  } else {
    throwError('unrecognized expSet.');
    return (-1);
  };

  // Determine filenames of critical trials based on label and condition
  var filenames_CriticalTypical, filenames_CriticalShifted;
  if (label === 'S') {
    filenames_CriticalTypical = repeat('', numCriticalPerLabel).map(function(x, i) {
      return x.concat('SH', itemID_Critical[i], 'H') });
    filenames_CriticalShifted = repeat('A', numCriticalPerLabel).map(function(x, i) {
      return x.concat('S', itemID_Critical[i], condition) });
  } else if (label === 'SH') {
    filenames_CriticalTypical = repeat('', numCriticalPerLabel).map(function(x, i) {
      return x.concat('S', itemID_Critical[i], 'H') });
    filenames_CriticalShifted = repeat('A', numCriticalPerLabel).map(function(x, i) {
      return x.concat('SH', itemID_Critical[i], condition) });
  } else if (label === 'all_typical') {
    // randomize which typical sound will be stored in filenames_CriticalShifted and which
    // will be stored in filenames_CriticalTypical (but both will contain only typical stimuli)
    // (this shouldn't have any consequences since stimuli are randomized below anyway, but
    // we are randomizing this aspect so that everything is randomized)
    if (Math.random() > .5) {
      filenames_CriticalTypical = repeat('', numCriticalPerLabel).map(function(x, i) {
        return x.concat('SH', itemID_Critical[i], condition) });
      filenames_CriticalShifted = repeat('', numCriticalPerLabel).map(function(x, i) {
        return x.concat('S', itemID_Critical[i], condition) });
    } else {
      filenames_CriticalTypical = repeat('', numCriticalPerLabel).map(function(x, i) {
        return x.concat('S', itemID_Critical[i], condition) });
      filenames_CriticalShifted = repeat('', numCriticalPerLabel).map(function(x, i) {
        return x.concat('SH', itemID_Critical[i], condition) });
    }
  // NOT YET HANDLED!
  } else if (label === 'no_critical') {
    throwError("label condition no_SSH not yet implemented.");
  } else if (label === 'no_exposure') {
    throwError("this function (get_pseudorandomized_exposure_list) should not be called in the no_exposure condition. Something went wrong.");
  } else {
    throwError("unrecognized label value.");
    return (-1);
  }

  /* create and concatenate block_length 'blocks' of exposure stimuli, where each block
     contains 1 critical typical, 1 critical shifted, and a proportional amount of word
     and non-word fillers (e.g., for expSet A & B: 3 filler words, 5 filler non-words;
     for expSet C & D: 1 filler word and 3 filler non-words).
     (and one CATCH trial that is either on a filler word or a filler non-word) */
  var catchIsFN, block, current_shifted, current_typical;
  var numFillerNonWords = (filenames_FillerNonWord.length + filenames_Catch.length / 2) / 10;
  var numFillerWords = (filenames_FillerWord.length + filenames_Catch.length / 2) / 10;
  var exposure_list = [];
  for (l = 0; l < 10; l++) {
    block = [];
    catchIsFN = 0;

    // store current typical and shifted so that we can make sure below that they do
    // not end up in the 10th position within the block.
    current_typical = filenames_CriticalTypical.randsplice(1);
    current_shifted = filenames_CriticalShifted.randsplice(1);
    block.push(current_typical);
    block.push(current_shifted);
    block.push(filenames_Catch.randsplice(1));
    block = block.flat(1);

    // figure whether catch trial is F or FN
    if (block[block.length-1].substring(0,2) === 'FN') catchIsFN = 1;
    block.push(filenames_FillerNonWord.randsplice(numFillerNonWords  - catchIsFN));
    block.push(filenames_FillerWord.randsplice(numFillerWords - (1 - catchIsFN)));

    // randomize order within block until the 10th trial is not typical or shifted
    do {
      block = shuffle(block.flat(1));
    } while (block[block.length - 1] === current_typical || block[block.length - 1] === current_shifted);
    if (block.length != block_length) throwError("during exposure list creation: Block " + (l + 1) + " does not have " + block_length + " elements.");

    exposure_list.push(block);
  }

  exposure_list = exposure_list.flat(1);
  throwMessage(exposure_list);

  return (exposure_list);
}


// this function was used for experiment A and NORM A, B
function get_pseudorandomized_test_list(testSet, testOcclusion) {
  // Makes the following assumptions:
  //   Item 1:6 are video condition H, Item 7:12 are video condition M
  //   Item 1:3 and 7:9 are extracted from S-videos, Item 4:6 and 10:12 are extracted from SH-videos
  var numBlock = 6;
  // the following sorts audio condition alphanumerically. Be aware that this will order 25 *after* 110.
  // for the current set of audio conditions, this is not a problem. If it ever becomes a problem, see
  // https://www.w3schools.com/js/js_array_sort.asp
  var audioCondition_unique, videoItemID_unique, videoFilename_suffix;
  if (testSet === 'A') {
    audioCondition_unique = ["12", "14", "15", "16", "17", "19"];
    videoItemID_unique = Array.from(new Array(12), (x, i) => i + 1);
  } else if (testSet === 'B') {
    // changed all but first audio step & removed video item 5, using a new (13) instead
    // NB: the order of the items in the array matters for the randomization, so it's important
    // to replace the new ID (13) where the old ID (5) would have been.
    audioCondition_unique = ["12", "16", "17", "18", "19", "22"];
    videoItemID_unique = ["1", "2", "3", "4", "13", "6", "7", "8", "9", "10", "11", "12"];
  } else if (testSet === 'C') {
    // testSet B unintentionally moved the continuum in the 'wrong' direction. TestSet C is an attempt
    // to remedy this by shifting the acoustic continuum in the intended direction. The video selection
    // is the same as in testSet B.
    // NB: the order of the items in the array matters for the randomization.
    audioCondition_unique = ["8", "12", "13", "14", "15", "19"];
    videoItemID_unique = ["1", "2", "3", "4", "13", "6", "7", "8", "9", "10", "11", "12"];
  } else {
    throwError('uncrecognized testSet.')
    return (-1);
  };

  if (testOcclusion) {
    videoFilename_suffix = '-occluder';
  } else {
    videoFilename_suffix = '';
  }

  var audioCondition = repeat(audioCondition_unique, numBlock * 2).flat(1).sort();
  var videoCondition = repeat([repeat(["H"], audioCondition_unique.length), repeat(["M"], audioCondition_unique.length)], numBlock).flat(3);
  var videoOriginalSound = repeat(repeat([repeat(["S"], 3), repeat(["SH"], 3)], 2), numBlock).flat(4);
  var videoItemID = repeat(videoItemID_unique, numBlock).flat(1);
  var videoFilename = audioCondition.map(function(x, i) {
    return x.concat(
      "_Frame",
      videoItemID[i], "-",
      videoCondition[i], "-",
      videoOriginalSound[i],
      videoFilename_suffix) })
  // create a Latin-square design of block assignments with a random (participant-specific) offset
  var order = new Array(numBlock * 12);
  var randomOffset = parseInt(Math.random() * numBlock);
  for (i = 0; i < 72; i++) {
    // final random number creates random sorting within each block; the part before the
    // random number creates a Latin-squared block assignment (0,...,5, 0,...,5, for block 1; 1,...,5,0, 1, ...,5,0, for block 2; 2...5,0,1, ... for block 3; etc.),
    // but with a random (participant-specific) offset determining the starting block for the first (and thereby) subsequent
    // blocks.
    order[i] = (i + randomOffset + Math.floor(i / 12)) % numBlock + Math.random();
  }

  // sort the filenames by the order created above
  var test_list = _.chain(order)
       .pairs()
       .sortBy(1)
       .map(function (i) { return videoFilename[i[0]]; })
       .value();

  return test_list;
}


// this function was used in LJ18, but we switched to online randomization for the CISP project
function get_exposure_list(list_num, label, condition, reverse) {
    var exposure_list = "";
    if (list_num === '1') {
        if (label === 'S') {
            if (condition === 'MOUTH') {
                exposure_list = ['FN95H', 'F39M-CATCH', 'FN37M', 'FN53M',
                                'AS17M', //
                                'F59M', 'FN55M',
                                'SH19H',
                                'FN67M-CATCH', 'FN71H',
                                'SH11H',
                                'F21H', 'F33H', 'FN7M', 'F55M', 'FN15M',
                                'AS15M', //
                                'F41H', 'FN47M', 'FN5H', 'F50M', 'FN73M', 'F27H', 'FN35H-CATCH', 'FN69H',
                                'AS5M',//
                                'FN25H', 'FN75H',
                                'SH17H',
                                'F57M-CATCH', 'FN1M', 'F11H',
                                'AS3M', //
                                'F7H', 'FN41H', 'FN79H-CATCH',
                                'SH7H',
                                'F25M', 'FN85H', 'FN77H', 'F45M', 'F49H', 'FN57H', 'F15H',
                                'SH3H',
                                'F31H-CATCH', 'FN43M',
                                'SH9H',
                                'FN65H', 'FN59H', 'FN11M',
                                'AS1M', //
                                'F35M', 'FN90H', 'FN83M', 'FN87M-CATCH',
                                'AS13M', //
                                'FN45M', 'F9H-CATCH', 'FN61H', 'F28M', 'FN23H', 'F13M',
                                'AS19M', //
                                'FN27M', 'F1M-CATCH', 'FN63M', 'FN39M', 'FN81H',
                                'SH5H',
                                'F5M', 'FN97M',
                                'SH15H',
                                'FN9M', 'FN13H', 'F47H',
                                'AS9M', //
                                'FN93H', 'F37H-CATCH', 'FN33H', 'F3M', 'F19H', 'FN99M',
                                'SH13H',
                                'FN49H', 'FN89M',
                                'SH1H',
                                'FN19H', 'FN29M',
                                'AS7M',//
                                'F43M', 'FN31M', 'F23M', 'FN3M-CATCH', 'FN17M', 'FN21H',
                                'AS11M',//
                                'F17H', 'FN51H-CATCH', 'F53H'];
                }
            if (condition === 'HAND') {
                exposure_list = ['FN95H', 'F39M-CATCH', 'FN37M', 'FN53M',
                                'AS17H', //
                                'F59M', 'FN55M',
                                'SH19H',
                                'FN67M-CATCH', 'FN71H',
                                'SH11H',
                                'F21H', 'F33H', 'FN7M', 'F55M', 'FN15M',
                                'AS15H', //
                                'F41H', 'FN47M', 'FN5H', 'F50M', 'FN73M', 'F27H', 'FN35H-CATCH', 'FN69H',
                                'AS5H',//
                                'FN25H', 'FN75H',
                                'SH17H',
                                'F57M-CATCH', 'FN1M', 'F11H',
                                'AS3H', //
                                'F7H', 'FN41H', 'FN79H-CATCH',
                                'SH7H',
                                'F25M', 'FN85H', 'FN77H', 'F45M', 'F49H', 'FN57H', 'F15H',
                                'SH3H',
                                'F31H-CATCH', 'FN43M',
                                'SH9H',
                                'FN65H', 'FN59H', 'FN11M',
                                'AS1H', //
                                'F35M', 'FN90H', 'FN83M', 'FN87M-CATCH',
                                'AS13H', //
                                'FN45M', 'F9H-CATCH', 'FN61H', 'F28M', 'FN23H', 'F13M',
                                'AS19H', //
                                'FN27M', 'F1M-CATCH', 'FN63M', 'FN39M', 'FN81H',
                                'SH5H',
                                'F5M', 'FN97M',
                                'SH15H',
                                'FN9M', 'FN13H', 'F47H',
                                'AS9H', //
                                'FN93H', 'F37H-CATCH', 'FN33H', 'F3M', 'F19H', 'FN99M',
                                'SH13H',
                                'FN49H', 'FN89M',
                                'SH1H',
                                'FN19H', 'FN29M',
                                'AS7H',//
                                'F43M', 'FN31M', 'F23M', 'FN3M-CATCH', 'FN17M', 'FN21H',
                                'AS11H',//
                                'F17H', 'FN51H-CATCH', 'F53H'];
                }
        }
        if (label === 'SH') {
            if (condition === 'MOUTH') {
                exposure_list = ['FN95H', 'F39M-CATCH', 'FN37M', 'FN53M',
                                'ASH17M', //
                                'F59M', 'FN55M',
                                'S19H',
                                'FN67M-CATCH', 'FN71H',
                                'S11H',
                                'F21H', 'F33H', 'FN7M', 'F55M', 'FN15M',
                                'ASH15M', //
                                'F41H', 'FN47M', 'FN5H', 'F50M', 'FN73M', 'F27H', 'FN35H-CATCH', 'FN69H',
                                'ASH5M',//
                                'FN25H', 'FN75H',
                                'S17H',
                                'F57M-CATCH', 'FN1M', 'F11H',
                                'ASH3M', //
                                'F7H', 'FN41H', 'FN79H-CATCH',
                                'S7H',
                                'F25M', 'FN85H', 'FN77H', 'F45M', 'F49H', 'FN57H', 'F15H',
                                'S3H',
                                'F31H-CATCH', 'FN43M',
                                'S9H',
                                'FN65H', 'FN59H', 'FN11M',
                                'ASH1M', //
                                'F35M', 'FN90H', 'FN83M', 'FN87M-CATCH',
                                'ASH13M', //
                                'FN45M', 'F9H-CATCH', 'FN61H', 'F28M', 'FN23H', 'F13M',
                                'ASH19M', //
                                'FN27M', 'F1M-CATCH', 'FN63M', 'FN39M', 'FN81H',
                                'S5H',
                                'F5M', 'FN97M',
                                'S15H',
                                'FN9M', 'FN13H', 'F47H',
                                'ASH9M', //
                                'FN93H', 'F37H-CATCH', 'FN33H', 'F3M', 'F19H', 'FN99M',
                                'S13H',
                                'FN49H', 'FN89M',
                                'S1H',
                                'FN19H', 'FN29M',
                                'ASH7M',//
                                'F43M', 'FN31M', 'F23M', 'FN3M-CATCH', 'FN17M', 'FN21H',
                                'ASH11M',//
                                'F17H', 'FN51H-CATCH', 'F53H'];
                }
            if (condition === 'HAND') {
                exposure_list = ['FN95H', 'F39M-CATCH', 'FN37M', 'FN53M',
                                'ASH17H', //
                                'F59M', 'FN55M',
                                'S19H',
                                'FN67M-CATCH', 'FN71H',
                                'S11H',
                                'F21H', 'F33H', 'FN7M', 'F55M', 'FN15M',
                                'ASH15H', //
                                'F41H', 'FN47M', 'FN5H', 'F50M', 'FN73M', 'F27H', 'FN35H-CATCH', 'FN69H',
                                'ASH5H',//
                                'FN25H', 'FN75H',
                                'S17H',
                                'F57M-CATCH', 'FN1M', 'F11H',
                                'ASH3H', //
                                'F7H', 'FN41H', 'FN79H-CATCH',
                                'S7H',
                                'F25M', 'FN85H', 'FN77H', 'F45M', 'F49H', 'FN57H', 'F15H',
                                'S3H',
                                'F31H-CATCH', 'FN43M',
                                'S9H',
                                'FN65H', 'FN59H', 'FN11M',
                                'ASH1H', //
                                'F35M', 'FN90H', 'FN83M', 'FN87M-CATCH',
                                'ASH13H', //
                                'FN45M', 'F9H-CATCH', 'FN61H', 'F28M', 'FN23H', 'F13M',
                                'ASH19H', //
                                'FN27M', 'F1M-CATCH', 'FN63M', 'FN39M', 'FN81H',
                                'S5H',
                                'F5M', 'FN97M',
                                'S15H',
                                'FN9M', 'FN13H', 'F47H',
                                'ASH9H', //
                                'FN93H', 'F37H-CATCH', 'FN33H', 'F3M', 'F19H', 'FN99M',
                                'S13H',
                                'FN49H', 'FN89M',
                                'S1H',
                                'FN19H', 'FN29M',
                                'ASH7H',//
                                'F43M', 'FN31M', 'F23M', 'FN3M-CATCH', 'FN17M', 'FN21H',
                                'ASH11H',//
                                'F17H', 'FN51H-CATCH', 'F53H'];
                            }
        }

        if (condition === 'FILLER') {
            exposure_list = ['FN95H', 'F39M-CATCH', 'FN37M', 'FN53M',
                            'F12H', //
                            'F59M', 'FN55M',
                            'F32M',
                            'FN67M-CATCH', 'FN71H',
                            'F42M',
                            'F21H', 'F33H', 'FN7M', 'F55M', 'FN15M',
                            'F51H', //
                            'F41H', 'FN47M', 'FN5H', 'F50M', 'FN73M', 'F27H', 'FN35H-CATCH', 'FN69H',
                            'F54H',//
                            'FN25H', 'FN75H',
                            'F14M',
                            'F57M-CATCH', 'FN1M', 'F11H',
                            'F44H', //
                            'F7H', 'FN41H', 'FN79H-CATCH',
                            'F26M',
                            'F25M', 'FN85H', 'FN77H', 'F45M', 'F49H', 'FN57H', 'F15H',
                            'F46M',
                            'F31H-CATCH', 'FN43M',
                            'F56M',
                            'FN65H', 'FN59H', 'FN11M',
                            'F52H', //
                            'F35M', 'FN90H', 'FN83M', 'FN87M-CATCH',
                            'F60H', //
                            'FN45M', 'F9H-CATCH', 'FN61H', 'F28M', 'FN23H', 'F13M',
                            'F48H', //
                            'FN27M', 'F1M-CATCH', 'FN63M', 'FN39M', 'FN81H',
                            'F18M',
                            'F5M', 'FN97M',
                            'F58M',
                            'FN9M', 'FN13H', 'F47H',
                            'F29H', //
                            'FN93H', 'F37H-CATCH', 'FN33H', 'F3M', 'F19H', 'FN99M',
                            'F30M',
                            'FN49H', 'FN89M',
                            'F40M',
                            'FN19H', 'FN29M',
                            'F36H',//
                            'F43M', 'FN31M', 'F23M', 'FN3M-CATCH', 'FN17M', 'FN21H',
                            'F24H',//
                            'F17H', 'FN51H-CATCH', 'F53H'];
            }
    }

    if (reverse === 'Y') {
        exposure_list.reverse();
    }
    return exposure_list;
}

// this function was used in LJ18, but we switched to online randomization for the CISP project
function get_test_list(test_reverse) {
  var test_list = "";
  if (test_reverse === 'N') {
    test_list = ["14H_Item5", "15M_Item10",
				 "19H_Item1", "15H_Item4",
				 "16M_Item9", "12H_Item6",
				 "16H_Item3", "17H_Item2",
				 "12M_Item12", "14M_Item11",
				 "17M_Item8", "19M_Item7",
				 "14H_Item6", "14M_Item12",
				 "19M_Item8", "17M_Item9",
				 "19H_Item2", "17H_Item3",
				 "12H_Item1", "15H_Item5",
				 "16H_Item4", "16M_Item10",
				 "15M_Item11", "12M_Item7",
				 "16M_Item11", "19H_Item3",
				 "12H_Item2", "19M_Item9",
				 "15M_Item12", "14H_Item1",
				 "16H_Item5", "17H_Item4",
				 "17M_Item10", "14M_Item7",
				 "12M_Item8", "15H_Item6",
				 "17M_Item11", "17H_Item5",
				 "12M_Item9", "14H_Item2",
				 "12H_Item3", "15M_Item7",
				 "16M_Item12", "14M_Item8",
				 "19H_Item4", "19M_Item10",
				 "15H_Item1", "16H_Item6",
				 "19H_Item5", "17M_Item12",
				 "19M_Item11", "14M_Item9",
				 "15M_Item8", "12H_Item4",
				 "14H_Item3", "12M_Item10",
				 "17H_Item6", "16H_Item1",
				 "16M_Item7", "15H_Item2",
				 "14M_Item10", "15M_Item9",
				 "17M_Item7", "12H_Item5",
				 "17H_Item1", "19M_Item12",
				 "14H_Item4", "12M_Item11",
				 "16M_Item8", "19H_Item6",
				 "15H_Item3", "16H_Item2"];
  }

  if (test_reverse === 'Y') {
    test_list.reverse();
  }

  return test_list;
}
