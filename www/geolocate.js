$(function(){
  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(function(position){
      $("#latitude").val(position.coords.latitude);
      $("#longitude").val(position.coords.longitude);
    });
  } else {
    $("#address-wrapper").show();
  }
});