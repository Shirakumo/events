/*
            <div class="duration local">
              <i class="fa fa-map-marker fa-fw"></i>
              <time class="local start">Thursday 21:00</time>
              -
              <time class="local end">Thursday 23:00</time>
              <span class="timezone">WEST</span>
              </div>
*/

function init(){
    function formatDateTime(date){
        var p = function(a){return(a<10)?"0"+a:a};
        return date.getFullYear()
            +"-"+p(date.getMonth())
            +"-"+p(date.getDate())
            +"T"+p(date.getHours())
            +":"+p(date.getMinutes());
    }

    function fetchLocation(callback) {
        var xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function () {
            if (this.readyState == 4 && this.status == 200) {
                callback(JSON.parse(xhttp.responseText));
            }
        };
        xhttp.open("GET", "http://ip-api.io/json/", true);
        xhttp.send();
    }

    var editor = document.getElementById("editor");

    if(editor){
        var start = document.getElementById("start");
        var location = document.getElementById("location");
        
        if(start.value === ""){
            start.value = formatDateTime(new Date());
        }

        if(location.value === ""){
            fetchLocation(function(data){
                console.log(data);
                location.value = data.region_name+", "+data.country_name;
            });
        }
    }

}

init();
