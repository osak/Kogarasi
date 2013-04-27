kogarasi_path = "http://localhost/~osak/CGIMain.fcgi";
function kogarasi_post(slug) {
    var name = $('#kogarasi-name-' + slug).val();
    var body = $('#kogarasi-body-' + slug).val();
    var param = {"action": "store", "name": name, "body": body, "slug": slug};
    $.ajax({
        url: kogarasi_path,
        data: param,
        dataType: 'json',
        type: 'POST'
    }).done(function(data) {
    }).fail(function() {
    }).always(function() {
    });
}

$(document).ready(function() {
    $('.kogarasi-mark').each(function(index) {
        var idstr = $(this).attr("id");
        if(idstr.match(/^kogarasi-(.*)$/)) {
            var slug = RegExp.$1;
            $.ajax({
                url: kogarasi_path,
                data: {"action": "fetch", "slug": slug},
                context: $(this),
                dataType: 'json',
                type: 'POST'
            }).done(function(data) {
                if(data.length == 0) {
                    $(this).append("<p>No comments</p>");
                } else {
                    for(var i = 0; i < data.length; ++i) {
                        var entry = data[i];
                        var utcDate = new Date(entry.posted_posix*1000);
                        var html = '<div class="kogarasi-comment">'
                                   + '<div class="kogarasi-name">' + entry.name + '</div>'
                                   + '<div class="kogarasi-posted">' + utcDate.toLocaleDateString() + '</div>'
                                   + '<div class="kogarasi-body">' + entry.body + '</div>'
                                   + '</div>';
                        $(this).append(html);
                    }
                }
                $(this).append('<div class="kogarasi-form">'
                               + '<form action="">'
                               + 'Name: <input type="text" id="kogarasi-name-' + slug + '" /><br />'
                               + '<span style="vertical-align: top">Body:</span> <textarea rows="5" id="kogarasi-body-' + slug + '"></textarea><br />'
                               + '<input type="button" value="Submit" onclick="javascript:kogarasi_post(\'' + slug + '\')"/>'
                               + '</form></div>');
            });
        }
    });
});
