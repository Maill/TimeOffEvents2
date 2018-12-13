/****************************************************************************************************/

if(document.getElementById('logonForm')) document.getElementById('logonForm').addEventListener('submit', submitLogonForm);

/****************************************************************************************************/

function submitLogonForm(event)
{
  if(document.getElementById('logonFormBackground')) return;

  event.preventDefault();

  var background  = document.createElement('div');
  var information = document.createElement('div');

  background      .setAttribute('id', 'logonFormBackground');
  background      .setAttribute('class', 'informationBackground');

  information     .setAttribute('class', 'informationInfoType');

  information     .innerText = 'Checking credentials. Please wait...';

  document.body   .appendChild(background);

  document.getElementById('informationContainer').appendChild(information);

  $.ajax(
  {
    method: 'POST', dataType: 'json', timeout: 10000, data: JSON.stringify({ UserName: document.getElementById('usernameInput').value, Password: document.getElementById('passwordInput').value }), url: '/',

    error: (xhr, textStatus, errorThrown) =>
    {
      information.remove();
      background.remove();

      return xhr.responseJSON != undefined
      ? displayError(xhr.responseJSON, 'logonFormError')
      : displayError('An error occurred. Please try again later', 'logonFormError');
    }

  }).done((result) =>
  {
    document.cookie = 'timeOffAuth=' + result.token + '; max-age=' + (60 * 60 * 24);

    location = '/home';
  });
}

/****************************************************************************************************/