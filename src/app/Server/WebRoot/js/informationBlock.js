/****************************************************************************************************/

function removeMessageFromTag(messageTag)
{
  const currentMessages = document.getElementById('informationContainer').children;

  for(var x = 0; x < currentMessages.length; x++)
  {
    if(currentMessages[x].getAttribute('tag') === messageTag) return currentMessages[x].remove();
  }
}

/****************************************************************************************************/

function displayInfo(message, messageTag)
{
  if(document.getElementById('informationContainer') == null) return;

  if(messageTag != null) removeMessageFromTag(messageTag);

  var block = document.createElement('div');

  block.setAttribute('class', 'informationInfoType');

  if(messageTag != null) block.setAttribute('tag', messageTag);

  block.innerHTML += `<div class="informationMessage">${message}</div>`;
  block.innerHTML += `<button onclick="remove(this)" class="informationClose">CLOSE</div>`;

  $(block).hide().appendTo(document.getElementById('informationContainer'));

  $(block).slideDown(250, () =>
  {
    setTimeout(() => { $(block).slideUp(250, () =>{ block.remove(); }); }, 10000);
  });
}

/****************************************************************************************************/

function displayError(message, messageTag)
{
  if(document.getElementById('informationContainer') == null) return;

  if(messageTag != null) removeMessageFromTag(messageTag);

  var block = document.createElement('div');

  block.setAttribute('class', 'informationErrorType');

  if(messageTag != null) block.setAttribute('tag', messageTag);

  block.innerHTML += `<div class="informationMessage">${message}</div>`;
  block.innerHTML += `<button onclick="remove(this)" class="informationClose">CLOSE</div>`;

  $(block).hide().appendTo(document.getElementById('informationContainer'));

  $(block).slideDown(250, () =>
  {
    setTimeout(() => { $(block).slideUp(250, () =>{ block.remove(); }); }, 60000);
  });
}

/****************************************************************************************************/

function displaySuccess(message, messageTag)
{
  if(document.getElementById('informationContainer') == null) return;

  if(messageTag != null) removeMessageFromTag(messageTag);

  var block = document.createElement('div');

  block.setAttribute('class', 'informationSuccessType');

  if(messageTag != null) block.setAttribute('tag', messageTag);

  block.innerHTML += `<div class="informationMessage">${message}</div>`;
  block.innerHTML += `<button onclick="remove(this)" class="informationClose">CLOSE</div>`;

  $(block).hide().appendTo(document.getElementById('informationContainer'));

  $(block).slideDown(250, () =>
  {
    setTimeout(() => { $(block).slideUp(250, () =>{ block.remove(); }); }, 10000);
  });
}

/****************************************************************************************************/