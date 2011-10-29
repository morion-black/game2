function runComet() {
  $.ajax({
    url: "/comet?user_id="+15,
    type: 'post',
    data: {},
    cache: false,
    success: function(data, textStatus) {
      eval(data);
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