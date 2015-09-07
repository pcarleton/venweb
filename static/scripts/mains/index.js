require(
        ["jquery",],
        function($) {

        var goToGraph = function() {
            var inputName = $("#username-box").val();

            // Strip @ sign
            if (inputName.indexOf("@") == 0) {
                inputName = inputName.slice(1);
            }

            if (inputName.length == 0) {
                return;
            }

            window.location.href = "/graph/" + $.trim(inputName);
        }

        // Hack to get click events on mobile
        document.getElementById("submit-username").onclick = goToGraph;

        $('#username-box').keyup(function(e){
            if(e.keyCode == 13) {
                goToGraph();
            }
        });
});
