
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
        var payload = {type: "chat_message", body: this.post, name: username, chan: chan};
        $.post("/message", JSON.stringify(payload));
        this.post = "";
      },
      receivedMessage: function(data){
        if(data.type == "chat_message"){
          data.body = emojione.toImage(data.body);
        }
        this.messages.push(data);
        $(document).scrollTop($(document).height());
      },
      setUsername: function() {
        this.username = window.username;
        $('#chatbox input').focus();
        var payload = {type: "join", name: this.username, chan: chan};
        $.post("/message", JSON.stringify(payload));
        console.log("setting username");
      }
    },
    mounted: function() {
      Event.$on("message", this.receivedMessage);
      this.setUsername();
    }

  });

  evtSource.onmessage = function(event) {
    var data = JSON.parse(event.data)
    console.log("evtSource incoming: " + event.data)
    Event.$emit("message", JSON.parse(event.data));
  };

});

Vue.component('message', {
  template: `
    <li><slot></slot></li>
  `
});
