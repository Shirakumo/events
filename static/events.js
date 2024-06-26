var Events = function(){
    var self = this;

    self.formatCountdown = function(secs){
        secs = Math.max(0, secs);
        var p = function(a){return(a<10)?"0"+a:a;};
        var s = Math.floor(secs/1) % 60;
        var m = Math.floor(secs/60) % 60;
        var h = Math.floor(secs/(60*60)) % 24;
        var d = Math.floor(secs/(60*60*24)) % 365;
        var y = Math.floor(secs/(60*60*24*365));
        return ""
            +((y==0)?"":y+"y ")
            +((y==0&d==0)?"":d+"d ")
            +((y==0&d==0&&h==0)?"":h+":")
            +((y==0&d==0&&h==0&&m==0)?"":p(m)+":")
            +((y==0&d==0&&h==0&&m==0&&s==0)?"":p(s));
    };

    self.formatDateTimeValue = function(date){
        var p = function(a){return(a<10)?"0"+a:a;};
        return date.getFullYear()
            +"-"+p(date.getMonth()+1)
            +"-"+p(date.getDate())
            +"T"+p(date.getHours())
            +":"+p(date.getMinutes());
    };

    self.formatHumanDate = function(date){
        var p = function(a){return(a<10)?"0"+a:a;};
        return date.getFullYear()
            +"."+p(date.getMonth()+1)
            +"."+p(date.getDate())
            +" "+p(date.getHours())
            +":"+p(date.getMinutes())
            +":"+p(date.getSeconds());
    };

    self.fetchLocation = function(callback) {
        var xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function () {
            if (this.readyState == 4 && this.status == 200) {
                callback(JSON.parse(xhttp.responseText));
            }
        };
        xhttp.open("GET", "//ip-api.io/json/", true);
        xhttp.send();
        return xhttp;
    };

    self.initEditor = function(editor){
        var start = editor.querySelector("#start");
        var location = editor.querySelector("#location");
        
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
    };

    self.createElement = function(tag, classes, content){
        var element = document.createElement(tag);
        element.className = classes;
        element.innerHTML = content || "";
        return element;
    };

    self.localTimeZoneAbbreviation = function(date){
        return (date||new Date()).toLocaleTimeString('en-us',{timeZoneName:'short'}).split(' ')[2];
    };

    self.localDate = function(date){
        return new Date(Date.parse(date));
    };

    self.dateTimeDifference = function(a, b){
        return (a.getTime() - b.getTime()) / 1000;
    };

    self.addLocalDate = function(event){
        var durations = event.querySelector(".durations");
        var start = durations.querySelector(".duration.author .start").getAttribute("datetime");
        var end = durations.querySelector(".duration.author .end").getAttribute("datetime");
        var div = self.createElement("div", "duration local");
        div.appendChild(self.createElement("i", "fa fa-map-marker fa-fw"));
        div.appendChild(self.createElement("time", "local start", self.formatHumanDate(self.localDate(start))));
        div.appendChild(document.createTextNode(" - "));
        div.appendChild(self.createElement("time", "local end", self.formatHumanDate(self.localDate(end))));
        div.appendChild(self.createElement("span", "timezone", self.localTimeZoneAbbreviation(self.localDate(start))));
        durations.appendChild(div);
        return event;
    };

    self.updateCountdown = function(event){
        var countdown = event.querySelector(".countdown");
        var time = self.localDate(event.querySelector(".duration.author .start").getAttribute("datetime"));
        var diff = self.dateTimeDifference(time, new Date());
        countdown.innerText = self.formatCountdown(diff);
        return diff;
    };

    self.startCountdownUpdate = function(event){
        if(event.querySelector(".countdown") && event.querySelector(".countdown").innerText.trim() != ""){
            setInterval(function(){
                if(self.updateCountdown(event) <= 0){
                    window.location.href = window.location.href+"?t="+(new Date().getTime()/1000);
                }
            }, 1000);
        };
    };

    self.initEvent = function(event){
        self.addLocalDate(event);
        self.startCountdownUpdate(event);
    };

    self.initThemeButton = function(button){
        var theme = button.getAttribute("for");
        button.addEventListener("click", function(){
            window.localStorage.setItem("theme", theme);
        });
    };

    self.restoreTheme = function(){
        var button = window.localStorage.getItem("theme");
        if(button){
            document.getElementById(button).click();
        }
    };

    self.init = function(){    
        var editor = document.getElementById("editor");
        if(editor) self.initEditor(editor);
        
        var events = document.getElementsByClassName("event");
        for(var event of events){
            self.initEvent(event);
        }

        self.restoreTheme();
        var themes = document.getElementsByClassName("theme");
        for(var theme of themes){
            self.initThemeButton(theme);
        }
    };
};

var events = new Events();
events.init();
