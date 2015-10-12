require(
        ["jquery",
         "text!templates/warning.html",
         "text!templates/verifying.html",
         "text!templates/search_default.html",
        ],
        function($, warnTemplate, verifyTemplate, searchDefaultTemplate) {

        var userName = "";


        var storeUserName = function() {
            userName = $("#username-box").val();

            // Strip @ sign
            if (userName.indexOf("@") == 0) {
                userName = userName.slice(1);
            }
            userName = $.trim(userName);
        };

        var goToGraph = function() {
            window.location.href = "/graph/" + $.trim(userName);
        };

        var checkUsername = function() {
            if (userName.length == 0) {
                showError();
                return;
            }

            $.get("/ping/"+userName, function(resp) {
                if (resp == "present") {
                    goToGraph();
                } else {
                    showError();
                }
            }).error(function() {
                showError();
            });
        };

        var validate = function() {
            storeUserName();
            $(".search-container").html(verifyTemplate);
            checkUsername();
        }

        var showError = function() {
            $(".search-container").html(searchDefaultTemplate);
            $("#username-box").val(userName);

            setupListeners();

            $(".warning").html(warnTemplate);
            $(".warning").toggle();
        }

        var hideWarning = function() {
            $(".warning").slideUp();
        }

        var setupListeners = function() {
            // Hack to get click events on mobile
            document.getElementById("submit-username").onclick = validate;

            $('#username-box').keyup(function(e){
                if(e.keyCode == 13) {
                    validate();
                }
            });
        }

        var toggleHelp = function() {
            $(".help").toggle();
        };

        document.getElementById("uname-question").onclick = toggleHelp;

        setupListeners();
});
