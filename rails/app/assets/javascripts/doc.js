function doc_export_show() {
    $('.doc-export-checkbox').css('display', 'inline')
    $('.doc-export-show').css('display', 'none')
    $('.doc-export-hide').css('display', 'inline')
    $('.doc-export-export').css('display', 'inline')
}
function doc_export_hide() {
    $('.doc-export-checkbox').css('display', 'none')
    $('.doc-export-show').css('display', 'inline')
    $('.doc-export-hide').css('display', 'none')
    $('.doc-export-export').css('display', 'none')
}
function doc_export_export() {
    var ids = $('input[class=doc-export-checkbox]:checked').map(function() {
        return $(this).val()
    }).get().join(',')
    window.location.href = 'docs-export?q=' + ids
}
