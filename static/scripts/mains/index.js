require(
        ["jquery",],
        function($) {

        var goToGraph = function() {
            var inputName = $("#username-box").val();

            // Strip @ sign
            if (inputName.startsWith("@")) {
                inputName = inputName.slice(1);
            }

            if (inputName.length == 0) {
                return;
            }

            window.location.href = "/graph/" + inputName;
        }

        $("#submit-username").click(goToGraph);

        $('#username-box').keyup(function(e){
            if(e.keyCode == 13) {
                goToGraph();
            }
        });
});
