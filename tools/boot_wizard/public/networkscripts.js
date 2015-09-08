
//adds empty range to table
function addRange(name) {
  $('#'+name+"_range").append($('<tr><td><input type="text" placeholder="Name"/></td><td><input type="text" placeholder="First"/></td><td><input type="text" placeholder="Last"/></td><td><button onclick="removeRange(this)">Remove</button></td></tr>'))
  console.log('added ' + name)
}

//removes ranges from tables
function removeRange(obj) {
  $(obj).parent().parent().remove();
}

//list of networks
var networks = ["<%= @networks.map{|n|n['name']}*'","'%>"]

function createJSON(name) {
  var data = {
    'name': $('#'+name+'_name').val(),
    "category": $('#'+name+'_category').val(),
    "group": $('#'+name+'_group').val(),
    'deployment': $('#'+name+'_deployment').val(),
    'conduit': $('#'+name+'_conduit').val(),
    'v6prefix': $('#'+name+'_v6prefix').val(),
    'ranges': [ ],
    'router' : {
      'address' : $('#'+name+'_routeraddress').val(),
      'pref' : $('#'+name+'_routerpref').val()
    }
  };
  $('#'+name+"_range").children().each(function(i, e) {
    var children = $(e).children();
    data.ranges.push({
      "name": $(children[0]).children()[0].value,
      "first": $(children[1]).children()[0].value,
      "last": $(children[2]).children()[0].value
    })
  });
  return data;
}

//uploades json to sinatra server
function saveData(name) {
  var json = createJSON(name);
  var filename = json.name;

  var xhr = new XMLHttpRequest();

  xhr.open('POST', 'save/'+filename, true);
  xhr.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');

  // send the collected data as JSON
  xhr.send(JSON.stringify(json));

  xhr.onloadend = function () {
    location.reload();
  };
}