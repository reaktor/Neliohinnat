function links2Plots(){ 
    //$("#plot_list a").css("color","red");
    $("#plot_list a").click( function(){
	//$("#middle_page").html("AAA");
	$("#middle_page").load("/sandbox/aprt_price/source/aprtPricePlots.r",
			      {id : this.id} );
    });
 };

function attachForm(){
    // bind form using ajaxForm 
    $('#modelApplyForm').ajaxForm({ 
        // target identifies the element(s) to update with the server response 
        target: '#htmlPriceEstimate', 
 
        // success identifies the function to invoke when the server response 
        // has been received; here we apply a fade-in effect to the new content 
        success: function() { 
            $('#htmlPriceEstimate').fadeIn('slow'); 
        } 
    }); 
    //alert("attached");

}; 
 

$(document).ready( function(){
    $("a").click( function(e) { e.preventDefault() } ); 

    $("#main_page").load("/sandbox/aprt_price/brew/aprtPriceTabSummary.html");
    
    $("#nav_summary a").click( function(){
	$("#main_page").load("/sandbox/aprt_price/brew/aprtPriceTabSummary.html");
    });

    $("#nav_plots a").click( function(){
	$("#main_page").load("/sandbox/aprt_price/brew/aprtPriceTabPlots.html",
			     links2Plots);
    });

    $("#nav_apply_model a").click( function(){
	$("#main_page").load("/sandbox/aprt_price/brew/aprtPriceTabApplyModel.html",
			    attachForm);
    });

});

