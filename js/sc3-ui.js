function fetch_text_file(url) {
    var request = new XMLHttpRequest();
    request.addEventListener('load', () => document.getElementById('jsProgram').value = request.response);
    request.open('GET', url)
    request.send();
}

// Append timestamp to URL to defeat cache
function append_timestamp(url) {
    var ext = ((/\?/).test(url) ? '&' : '?') + (new Date()).getTime();
    return url + ext;
}

function load_graph(graphDir, graphName, fileType) {
    var graphUrl = 'help/' + graphDir + '/' + graphName + fileType;
    console.log(graphUrl);
    fetch_text_file(append_timestamp(graphUrl));
}

function menu_init(menuId, graphDir, fileType) {
    document.getElementById(menuId).addEventListener('change', (e) => load_graph(graphDir, e.target.value, fileType));
}

function sc3_init() {
    menu_init('graphMenu', 'graph', '.js');
    menu_init('helpMenu', 'ugen', '.js');
}
