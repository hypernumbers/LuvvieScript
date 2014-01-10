btn_click = function() {
    var response = test();
    console.log(response);
    var dv = document.getElementById('response');
    console.log(dv);
    dv.innerHTML = response;
};