$(document).ready(function() {
    $('.kogarasi-mark').each(function(index) {
        var idstr = $(this).attr("id");
        if(idstr.match(/^kogarasi-(.*)$/)) {
            var slug = RegExp.$1;
            $.ajax({
                url: "CGIMain.fcgi",
                data: {action: "fetch", slug: slug},
                context: $(this),
                dataType: 'json'
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
            });
        }
    });
});
