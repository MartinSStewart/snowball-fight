// port supermario_copy_to_clipboard_to_js : String -> Cmd msg

exports.init = async function(app) {
  document.addEventListener(
    "keydown",
    (event) => {
        if (event.key === 'Tab' || event.key === '\'' || event.key === '/' || event.key === 'F1')
        {
            event.preventDefault();
        }
    });

  window.addEventListener("mouseout", (event) => app.ports.mouse_leave.send());

}