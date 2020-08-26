const loaderClass = '.btn-loading-indicator';
const checkmarkClass = '.btn-done-indicator';
const errorClass = '.btn-error-indicator';

Shiny.addCustomMessageHandler('setButtonState', function(message) {
  const state = message[0];
  const buttonId = message[1];
  const element = document.getElementById(buttonId)
  console.log(element);
  if (state === 'loading') { showLoadState(element) }
  else if (state === 'done') { showDoneState(element) }
  else if (state === 'error') { showErrorState(element) }
  else if (state === 'none') { showNoState(element) }
});

function showLoadState(el) {
  el.parentNode.querySelector(loaderClass).style.display = 'inline-flex';
  el.parentNode.querySelector(checkmarkClass).style.display = 'none';
  el.parentNode.querySelector(errorClass).style.display = 'none';
}

function showDoneState(el) {
  el.parentNode.querySelector(loaderClass).style.display = 'none';
  el.parentNode.querySelector(checkmarkClass).style.display = 'inline-flex';
  el.parentNode.querySelector(errorClass).style.display = 'none';
}

function showErrorState(el) {
  el.parentNode.querySelector(loaderClass).style.display = 'none';
  el.parentNode.querySelector(checkmarkClass).style.display = 'none';
  el.parentNode.querySelector(errorClass).style.display = 'inline-flex';
}

function showNoState(el) {
  el.parentNode.querySelector(loaderClass).style.display = 'none';
  el.parentNode.querySelector(checkmarkClass).style.display = 'none';
  el.parentNode.querySelector(errorClass).style.display = 'none';
}
