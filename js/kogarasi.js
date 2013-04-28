kogarasi_path = "http://localhost/~osak/CGIMain.fcgi";
function kogarasi_post(slug) {
    var name = $('#kogarasi-name-' + slug).val();
    var body = $('#kogarasi-body-' + slug).val();
    var param = {"action": "store", "name": name, "body": body, "slug": slug};
    $.ajax({
        url: kogarasi_path,
        data: param,
        dataType: 'text',
        type: 'POST'
    }).done(function(data) {
    }).fail(function() {
    }).always(function() {
    });
}

function formatDate(date) {
    function padded(val) {
        if(val < 10) return "0" + val;
        return ""+val;
    }
    var y = date.getFullYear();
    var m = padded(date.getMonth()+1);
    var d = padded(date.getDate());
    var h = padded(date.getHours());
    var mi = padded(date.getMinutes());
    var s = padded(date.getSeconds());
    return y + "/" + m + "/" + d + " " + h + ":" + mi + ":" + s;
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
                        var u = new Date(entry.posted_posix*1000);
                        var dateString = formatDate(u);
                        var body = entry.body.replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\n/g, '<br />');
                        var html = '<div class="kogarasi-comment">'
                                   + '<div class="kogarasi-name">' + entry.name + '</div>'
                                   + '<div class="kogarasi-posted">' + dateString + '</div>'
                                   + '<div class="kogarasi-body">' + body + '</div>'
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
