require(
        ["jquery",],
        function($) {

        $("#submit-username").click(function() {
            var inputName = $("#username-box").val();

            // Strip @ sign
            if (inputName.startsWith("@")) {
                inputName = inputName.slice(1);
            }

            window.location.href = "/graph/" + inputName;
        });
});
