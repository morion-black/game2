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

function move(x, y) {
  $.get("/move", {game : window.game, user_id : window.user_id, x:x, y:y}, function(reply) {
    console.log(reply);
  });
}

$(function() {
  $.get("/game_info", {game : window.game}, function(game) {
    var i = 0, j = 0;
    var klass = "";
    
    var s = "<table class='slots'>";
    for(i = 0; i < game.size; i++) {
      s += "<tr>";
      for(j = 0; j < game.size; j++) {
        if(game.slots[i*3 + j]) {
          klass = "user"+game.slots[i*3 + j];
        } else {
          klass=""
        }
        s += "<td id='slot_"+i+"_"+j+"' class='"+klass+"'>"+
        "<a href='#' onclick='move("+i+","+j+"); return false'>&nbsp;</a>"+
        "</td>";
      }
      s += "</tr>";
    }
    
    s += "</table>";
    
    $("#slotsHolder").html(s);
    
    
    
    console.log(game);
  });
  
})

