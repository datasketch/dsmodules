const loaderClass = '.btn-loading-indicator';
const checkmarkClass = '.btn-done-indicator';

Shiny.addCustomMessageHandler('setButtonState', function(message) {
  const state = message[0];
  const buttonId = message[1];
  const element = document.getElementById(buttonId)
  if (state === 'loading') { showLoadState(element) }
  else if (state === 'done') { showDoneState(element) }
});

function showLoadState(el) {
  el.parentNode.querySelector(loaderClass).style.display = 'block';
  el.parentNode.querySelector(checkmarkClass).style.display = 'none';
}

function showDoneState(el) {
  el.parentNode.querySelector(loaderClass).style.display = 'none';
  el.parentNode.querySelector(checkmarkClass).style.display = 'block';
}
