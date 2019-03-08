// Fix clients on external screens so that they do not change
// when I change virtual desktops.
function updateClient(client) {
  if (workspace.numScreens == 1) {
    return;
  }

  if (client.screen == 0) {
    client.onAllDesktops = false;
  } else {
    client.onAllDesktops = true;
  }
}

function setUp(client) {
  client.screenChanged.connect(function () { updateClient(client); });

  updateClient(client);
}

// Set up new clients as they are created
workspace.clientAdded.connect(setUp);

// Fix all existing clients
workspace.clientList().forEach(setUp);

// Reset all clients when I disconnect the last external screen
workspace.numberScreensChanged.connect(function (count) {
  if (count == 1) {
    workspace.clientList().forEach(function (client) {
      client.onAllDesktops = false;
    });
  }
});
