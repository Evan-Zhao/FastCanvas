function move() {  
  var width = 0;
  var id = setInterval(frame, 500);
  function frame() {
    if (width >= 100) {
      clearInterval(id);
      $('#myP').html("Succeeded!");
      $('#myP')[0].className = "w3-text-green w3-animate-opacity";
    } else {
      width = width + 10; 
      $('#sync-bar')[0].style.width = width + '%'; 
      var num = width * 1 / 10;
      num = num.toFixed(0)
      $('#demo').html(num);
    }
  }
}

function label_rotate() {
  animateClass = "refresh-icon-animate";
  $('.refresh-icon').addClass( animateClass );
  // setTimeout is to indicate some async operation
  window.setTimeout( function() {
  	$('.refresh-icon').removeClass( animateClass );
  }, 2000 );
}

$('#sync-button')[0].onclick = function(){
  move();
  label_rotate();
}