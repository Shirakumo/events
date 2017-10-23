var Events = function(){
    var self = this;

    self.formatDateTimeValue = function(date){
        var p = function(a){return(a<10)?"0"+a:a};
        return date.getFullYear()
            +"-"+p(date.getMonth()+1)
            +"-"+p(date.getDate())
            +"T"+p(date.getHours())
            +":"+p(date.getMinutes());
    }

    self.formatHumanDate = function(date){
        var p = function(a){return(a<10)?"0"+a:a};
        return date.getFullYear()
            +"."+p(date.getMonth()+1)
            +"."+p(date.getDate())
            +" "+p(date.getHours())
            +":"+p(date.getMinutes())
            +":"+p(date.getSeconds());
    }

    self.fetchLocation = function(callback) {
        var xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function () {
            if (this.readyState == 4 && this.status == 200) {
                callback(JSON.parse(xhttp.responseText));
            }
        };
        xhttp.open("GET", "http://ip-api.io/json/", true);
        xhttp.send();
        return xhttp;
    }

    self.initEditor = function(editor){
        var start = editor.querySelector("#start");
        var location = editor.querySelector("#location");

        start.setAttribute("min", self.formatDateTimeValue(new Date()));
        
        if(start.value === ""){
            start.value = self.formatDateTimeValue(new Date());
        }

        if(location.value === ""){
            self.fetchLocation(function(data){
                console.log(data);
                location.value = data.region_name+", "+data.country_name;
            });
        }
        return editor;
    }

    self.createElement = function(tag, classes, content){
        var element = document.createElement(tag);
        element.className = classes;
        element.innerHTML = content || "";
        return element;
    }

    self.localTimeZoneAbbreviation = function(date){
        return (date||new Date()).toLocaleTimeString('en-us',{timeZoneName:'short'}).split(' ')[2];
    }

    self.localDate = function(date){
        return new Date(Date.parse(date+"Z"));
    }

    self.initEvent = function(event){
        var summary = event.querySelector(".summary");
        var start = summary.querySelector(".duration.author .start").getAttribute("datetime");
        var end = summary.querySelector(".duration.author .end").getAttribute("datetime");
        var div = self.createElement("div", "duration local");
        div.appendChild(self.createElement("i", "fa fa-map-marker fa-fw"));
        div.appendChild(self.createElement("time", "local start", self.formatHumanDate(self.localDate(start))));
        div.appendChild(document.createTextNode(" - "));
        div.appendChild(self.createElement("time", "local end", self.formatHumanDate(self.localDate(end))));
        div.appendChild(self.createElement("span", "timezone", self.localTimeZoneAbbreviation(self.localDate(start))));
        summary.appendChild(div);
        return event;
    }

    self.init = function(){    
        var editor = document.getElementById("editor");
        if(editor) self.initEditor(editor);
        
        var events = document.getElementsByClassName("event");
        for(var event of events){
            self.initEvent(event);
        }
    }

}

var events = new Events();
events.init();
