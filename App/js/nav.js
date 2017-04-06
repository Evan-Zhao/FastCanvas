function hideAllSectionsAndDeselectButtons () {
  const buttons = document.querySelectorAll('.nav-button.is-selected')
  Array.prototype.forEach.call(buttons, function (button) {
    button.classList.remove('is-selected')
  })
}

function handleSectionTrigger (event) {
  hideAllSectionsAndDeselectButtons()

  // Highlight clicked button and show view
  if ($(event.target).hasClass('nav-button'))
    event.target.classList.add('is-selected')
}

document.body.addEventListener('click', function (event) {
    handleSectionTrigger(event)
})
