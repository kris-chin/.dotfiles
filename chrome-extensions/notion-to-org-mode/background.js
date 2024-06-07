/*
  This service worker contains the logic to add the contextMenu action as well as the the HTTP call to the python server.
*/


//Callback function for onClick for the contextMenu item.
const onContextMenuClick = async (info) => {

  chrome.tabs.query({ active: true, currentWindow: true }, async (tabs) => {
    //only 1 tab should be returned.
    const activeTab = tabs[0];

    const url = activeTab.url
    const tab_id = activeTab.id

    //Put our url in the payload
    const body = JSON.stringify({
      url
    })

    //such ugly nested code..
    try {
      //Attempt to make our POST call to the server.
      const res = await fetch('http://localhost:5432/handle_click', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body
      })

      if (res.status === 500) {
        const json = await res.json()
        chrome.tabs.sendMessage(tab_id, { action: 'server_error', body: json })
        return
      }

      try {
        const json = await res.json()
        chrome.tabs.sendMessage(tab_id, { action: 'conversion_success', body: json })
      } catch (error) {
        chrome.tabs.sendMessage(tab_id, { action: 'misc_error' })
      }
    } catch (error) {
      chrome.tabs.sendMessage(tab_id, { action: 'fetch_fail' })
    }

  })

}

//Add the onClick callback to the contextMenus API
chrome.contextMenus.onClicked.addListener(onContextMenuClick)

//Run this when extension is installed
chrome.runtime.onInstalled.addListener(() => {
  //Add the context menu
  chrome.contextMenus.create({
    title: "Convert Notion to Org Mode",
    contexts: ["all"],
    id: 'all'
  })
})
