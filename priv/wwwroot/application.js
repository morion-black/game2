function parseQueryString() {
  var re1 = /^([^?]+)\?(.+)/
  var urlHalves;
  urlHalves = re1.exec(String(document.location));
  if(!urlHalves) {
    return {};
  }
  urlHalves = urlHalves[2];
  var params = {};
  var i;

  var re = /^([^=]+)=(.*)$/;

  var urlVars = urlHalves.split('&');
  for(i=0; i< urlVars.length; i++){
    var kv = re.exec(urlVars[i]);
    params[kv[1]] = kv[2];
  }
  return params;
}


var params = parseQueryString();
window.user_id = params["user_id"];
window.game = params["game"];

function runComet() {
  $.ajax({
    url: "/comet?user_id="+user_id+"&game=lala",
    type: 'post',
    data: {},
    cache: false,
    success: function(data, textStatus) {
      // eval(data);
      console.log(data);
      if(console) console.log("Restarting comet after success");
      setTimeout(runComet, 50);
    },
    
    error: function(xmlHttpRequest, textStatus, errorThrown) {
      if(console) console.log("Restarting comet after error");
      setTimeout(runComet, 50);
    }
  });
}
$(runComet);

function message(message) {
  $.get("/message", {message : message, game : window.game, user_id : window.user_id}, function(reply) {
    
  });
}


$(function() {
  $.get("/game_info", {game : window.game}, function(game) {
    console.log(game);
  });
  
})

