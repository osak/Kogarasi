kogarasiPath = "http://localhost/kogarasi"

function kogarasiExpand(slug) {
    var context = $('#kogarasi-' + slug);
    context.css('height', 'auto').css('overflow', '');
    console.log(context.children('.kogarasi-expand'));
    context.find('.kogarasi-expand').remove();
    context.find('.kogarasi-footer').css('position', 'static');
}

function kogarasiLoad(slug) {
    var context = $('#kogarasi-' + slug);
    var length = 0;
    $.ajax({
        url: kogarasiPath + "/show/" + slug,
        context: context,
        dataType: 'json',
        type: 'GET'
    }).done(function(data) {
        $(this).html('');
        if(data.length == 0) {
            $(this).append("<p>No comments</p>");
            empty = true;
        } else {
            for(var i = 0; i < data.length; ++i) {
                var entry = data[i];
                var u = new Date(entry.posted_posix*1000);
                var dateString = formatDate(u);
                var name = entry.name.replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\n/g, '<br />');
                var body = entry.body.replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\n/g, '<br />');
                body = body.replace(/(https?:\/\/[a-zA-Z0-9%.\/]+)/g, function(){return '<a href="' + RegExp.$1 + '">' + RegExp.$1 + '</a>'});
                var html = '<div class="kogarasi-comment">'
                           + '<div class="kogarasi-name">' + name + '</div>'
                           + '<div class="kogarasi-posted">' + dateString + '</div>'
                           + '<div class="kogarasi-body">' + body + '</div>'
                           + '</div>';
                $(this).append(html);
            }
            length = data.length;
        }
    }).fail(function() {
        $(this).append("<p>Failed to fetch comments.</p>");
    }).always(function() {
        var container = $(this);
        if(length > 0) {
            container = $('<div class="kogarasi-footer"><div class="kogarasi-expand">'
                + '<span class="kogarasi-expand-text"><a href="#" onclick="javascript:kogarasiExpand(\'' + slug + '\'); return false;">Show all ' + length + ' comments</a></span>'
                + '</div></div>');
            $(this).append(container);
        }
        container.append('<div class="kogarasi-form">'
            + '<form action="">'
            + 'Name: <input type="text" id="kogarasi-name-' + slug + '" /><br />'
            + '<span style="vertical-align: top">Body:</span> <textarea rows="5" id="kogarasi-body-' + slug + '"></textarea><br />'
            + '<input type="button" value="Submit" onclick="javascript:kogarasiPost(\'' + slug + '\')"/>'
            + '</form></div>');
    });
}

function kogarasiPost(slug) {
    var name = $('#kogarasi-name-' + slug).val();
    var body = $('#kogarasi-body-' + slug).val();
    var param = {"name": name, "body": body, "slug": slug};
    $.ajax({
        url: kogarasiPath + "/post",
        data: param,
        dataType: 'text',
        type: 'POST'
    }).done(function(data) {
        kogarasiLoad(slug);
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
            kogarasiLoad(slug);
        }
    });
});
