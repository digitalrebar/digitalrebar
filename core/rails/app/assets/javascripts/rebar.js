(function(a){a.fn.konami=function(b,c){c=a.extend({},a.fn.konami.params,c);this.each(function(){var d=a(this);d.bind("konami",b).bind("keyup",function(e){a.fn.konami.checkCode(e,c,d);});});return this;};a.fn.konami.params={code:[38,38,40,40,37,39,37,39,66,65],step:0};a.fn.konami.checkCode=function(b,c,d){if(b.keyCode==c.code[c.step]){c.step++;}else{c.step=0;}if(c.step==c.code.length){d.trigger("konami");c.step=0;}};})(jQuery);
var piechart_options = {type: 'pie', width: '17px', height: '17px', sliceColors: ["#0f0", "#f00", "#999", "#ff0"] };

/**
 * Utility method stripping any html out of a localized string.  In particular if the
 * key is not present in the localization file you'll see something like:
 * <span class="missing_transaltion">need,to,strip,html</span> which will break your javascript.
 *
 * @usage $.localizedValue("<span class="missing_transaltion">need,to,strip,html</span>");
 * @param value
 * @returns string
 */
jQuery.localizedValue = function(val){
	return $(val).text();
};

jQuery(document).ready(function($) {


	/**
	 * Utility method for retrieving a jQuery element by passing in the id of the element you want.
	 * Simply a shortcut.
	 *
	 * @usage $.getById("some_element_id");
	 * @param id
	 * @returns element
	 */
	jQuery.getById = function(id) {
		return $("#" + id);
	};

	/**
	 * Add load details behavior to anchor tags by class.
	 *
	 * @usage $.loadDetails(anchor_class, details_id);
	 * @param anchor_class, the anchor tags we want to add loadDetails on
	 * @param details_id, id of div we are going to load content into.
	 * @returns boolean
	 */
	jQuery.loadDetails = function(anchor_class, details_id) {
		$achors = $('a.' + anchor_class);
		$achors.click(function(e) {
			selected = $(this).attr('id');
			$.getById(details_id).load($(this).attr('href'));
			$('tr.selected', $(this).parent('table')).removeClass('selected');
			$(this).parent('tr').addClass('selected');
			e.preventDefault();
		});
		return true;
	};

  $('textarea.editor').each(function(index) {
    CodeMirror.fromTextArea(this, {
      lineNumbers: true,
      matchBrackets: true
    });
  });

  $('.inline_piechart').sparkline('html', piechart_options );

  // Blinking lights
  setInterval( function() {
    $('.led.failed, .led.pending, .led.waiting, led.red').toggleClass('blink');
  }, 500);

  // Animate spinning LEDs
  function animate() {
    $('.led.transition, .led.spin').sprite({fps: 6, no_of_frames: 8});
  }

  animate(); // Call this again when new animatable elements are created...

  // Auto-run update functions periodically
  if(typeof update == 'function') {
    setInterval(function() {
      update();
      animate();
      heartbeat();
    }, 10000);
  }

  $('.formtastic')
    .bind("ajax:error", function(evt, xhr, status, error){
      try {
        alert(JSON.parse(xhr.responseText)["message"]);
      }
      catch (e) {
        alert("Error without detail.  Could not parse error message from response: " + error);
      }
      $('.button').removeClass('pressed');
      })
    .bind("ajax:success", function(data, status, xhr){
      location.reload();
      });

  $('.button').live('click', function() {
    var button = $(this);
    button2 = $('.button[source="'+button.attr('match')+'"]');
    $('#flash').attr("style", "display:none");
    button.addClass('pressed');
    if (button2) button2.addClass('pressed');
    if(button.attr('data-remote')=='true') {
      button.bind('handleRemote', function(){ button.removeClass('pressed'); });
      if (button2) button2.bind('handleRemote', function(){ button2.removeClass('pressed'); });
    }
  });

  $('input[data-default]').each(function() {
    $(this).val($(this).attr('data-default'))
  })
  $('input[data-default-clear]').each(function() {
    $(this).val($(this).attr('data-default-clear')).addClass('default');
  }).click(function(e){
    $(this).val('').removeClass('default');
  });
  // Toggle stuff
  $('.toggle').not('.disabled').click(function() {
    targets = $(this).attr('rel').split(',');
    $(this).toggleClass('on');
    $.each(targets, function(index, target) {
      try { target = target.trim(); } catch(err) { true; }
      if( $(this).attr('data-speed') ) {
        $('#'+target).slideToggle( $(this).attr('data-speed') );
      } else {
        $('#'+target).toggle();
      }
    });
  });

  $(document).konami(function(){
    $("header h1 a").css('background-image','url("/assets/layout/dr_bunny.png")').css('width','279px');
  });
});
