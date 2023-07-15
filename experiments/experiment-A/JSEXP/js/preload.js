// to make this is an object
var finishedLoading = { val: 0 };
var alreadyInstalled = []
var stimuli_list_all = []

var onLoadProgress = function (event) {
  var p = this.stimuliObj.queue.progress * 100;
  document.querySelector('.preload-amount').textContent = " "+Math.round(p)+"%";

}

var onPreloadFile = function(event) {
    if (console) {
        console.log("Preloading:", event.item);
    }
    var item = event.item; // A reference to the item that was passed in to the LoadQueue
    var type = item.type;
    var ext = item.ext;
    // Add any images to the page body.
    if (type == 'video') {
 //       var video = document.createElement('video');
  //      video.src = item.src;
 //       video.setAttribute('class', item.css_class + ' vidStim');
//        video.setAttribute('style', 'display: none');
        if (alreadyInstalled.length > 0){
            if (alreadyInstalled[alreadyInstalled.length-1] != item.src) {
                alreadyInstalled.push(item.src);
                stimuli_list_all.push({'src':item.src, 'class':item.css_class + ' vidStim', 'type':'video'});
            //    document.body.children.videoContainer.appendChild(video);
            }
        }
        else {
            alreadyInstalled.push(item.src);
            stimuli_list_all.push({'src':item.src, 'class':item.css_class + ' vidStim', 'type':'video'});
          //  document.body.children.videoContainer.appendChild(video);

        }
    }
    //i'm using the ext instead of type. for some reaosn type returns 'text'
    if (ext  == 'wav' | ext == 'mp3' | ext == "ogg") {
//        var audio = document.createElement('audio');
 //       audio.src = item.src;
   //     audio.setAttribute('class', item.css_class + ' audStim');
        if (alreadyInstalled.length > 0){
            if (alreadyInstalled[alreadyInstalled.length-1] != item.src) {
                alreadyInstalled.push(item.src);
                stimuli_list_all.push({'src':item.src, 'class':item.css_class + ' audStim', 'type':'audio'});
                console.log(stimuli_list_all);
              //  document.body.children.audioContainer.appendChild(audio);
            }
        }
        else {
            alreadyInstalled.push(item.src);
            stimuli_list_all.push({'src':item.src, 'class':item.css_class + ' audStim', 'type':'audio'});
            //document.body.children.audioContainer.appendChild(audio);
        }
    }
}

var onPreloadComplete = function(event) {
    $('#continue').show();
    finishedLoading.val = 1;
}
