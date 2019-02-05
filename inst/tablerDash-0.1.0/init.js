$(function () {

  // select the first nav item by default at start
  $('#mymenu .nav-item:eq(0) a').addClass('active');
  $('#mymenu .nav-item:eq(0) a').tab('show');
  $('.container-fluid.tab-pane:eq(0)').addClass('active show');

  // handles shinyapps.io
  var workerId = $('base').attr('href');
  // ensure that this code does not locally
  if (typeof workerId != "undefined") {
    // get the initial page url
    var url = window.location.href;
    // list all tabs
    $("#mymenu .nav-item a").each(function(){
      // get the tablink href value
      var tablink = $(this).attr("href");
      // set the new url taking worker id as reference
      $(this).attr("href", url + workerId + tablink);
      console.log($(this));
    });
  }

  // handle when the user click on other items
  //$('.nav-item').on('click', function (event) {
  //  // activate the seletec tab item
  //  $target = $(event.target);
  //  $target.addClass('active');
  //  $target.tab('show');
  //  // inactivate all other nav items
  //  $target.parent().siblings().find('a').removeClass('active');
  //  // get the item index
  //  $index = $target.parent().index();
  //  // show the corresponding tab
  //  $related_tab = $('.container-fluid.tab-pane:eq('+ $index +')');
  //  $related_tab.addClass('active show');
  //  // hide other tabs
  //  $other_tabs = $related_tab.siblings();
  //  $other_tabs.removeClass('active show');
  //});

});
