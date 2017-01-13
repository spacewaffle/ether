
$(document).ready(function() {

  var loc = window.location, new_uri;

  var evtSource = new EventSource("/sse" + window.location.search);

  // extract chan from query param chan=n
  var match = window.location.search.match(/chan=([^&]+)/),
      chan = match && match[1];

  window.Event = new Vue();
  var chan = chan || "all"; // change later to real channel number

  var postComponent = new Vue({
    el: "#chatbox",
    data: {
      post: "",
      messages: [],
      username: ""
    },
    methods: {
      submit: function() {
        console.log("Submit: " + this.post);
        // TODO assign channels later
        var payload = {type: "chat_message", body: this.post, name: this.username, chan: chan};
        $.post("/message", JSON.stringify(payload), function(data){$vm0.receivedMessage(data)});
        this.post = "";
      },
      receivedMessage: function(data){
        console.log("receiving message");
        switch(data.type){
          case "chat_message":
            this.postMessage(data);
            break;
          case "askUsername":
            this.setUsername(data);
          break;
          case "redirect":
            window.location = window.location.host + data.url;
          break;
        }
      },
      postMessage: function(data){
        data.body = emojione.toImage(data.body);
        this.messages.push(data);
        $(document).scrollTop($(document).height());
      },
      setUsername: function(data) {
        console.log("setting username");
        this.username = data.name;
      },
      askUsername: function() {
        var payload = {type: "askUsername"};
        $.post("/message", JSON.stringify(payload));
        console.log("asking for username");
      }
    },
    mounted: function() {
      Event.$on("message", this.receivedMessage);
      this.askUsername();
      $('#chatbox input').focus();
    }

  });

  evtSource.onmessage = function(event) {
    var data = JSON.parse(event.data);
    console.log("evtSource incoming: " + event.data);
    Event.$emit("message", JSON.parse(event.data));
  };

});

Vue.component('message', {
  template: `
    <li><slot></slot></li>
  `
});
