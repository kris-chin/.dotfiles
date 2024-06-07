/*
  Content Script that contains a handler for our service worker's messages
  Used just for visual feedback of our script, such as if the server failed, or if the conversion was successful.
*/

const handleOnFetchFail = () => {
  alert("Non-500: Fetch to local server failed. Is it running?")
}

const handleOnSuccess = (body) => {
  const pageid = body.pageid
  alert(`Conversion for page "${pageid}" successful`)
}

const handleOnServerError = (body) => {
  const error = body.error
  console.error(body.error)
  alert(`Server error: "${error}"`)
}

const handleOnMiscError = () => {
  alert(`A misc error occurred`)
}

//Add handlers for different types of messages
chrome.runtime.onMessage.addListener((msg) => {
  switch (msg.action) {
    case "fetch_fail":
      handleOnFetchFail();
      break;
    case "conversion_success":
      handleOnSuccess(msg.body);
      break;
    case "server_error":
      handleOnServerError(msg.body);
      break;
    case "misc_error":
      handleOnMiscError();
      break;
    default:
      alert(`No handling for msg`)
      break;
  }
})
