# sauna

This is a toy ActivityPub inbox.
The goals for this project were to understand the ActivityPub spec and to get a
feel for writing a web server in PureScript.

## Workflow

Create and view an account

```
GET /api/v1/accounts
Accepts: applications/json

POST /api/v1/accounts
Content-Type: application/json
{ "username": "foo" }
```

Creating an account passively creates an ActivityPub Actor.

```
GET /users/:username
Accepts: application/activity+json
```

The Actor is discoverable via the webfinger endpoint. 

```
GET /.well-known/webfinger?resource=acct:username@domain`
Accepts: application/activity+json
```

Send a message to the Actor's inbox. 
The inbox uses HTTP signatures to verify the authenticity of the message, so all
requests must be signed.

```
POST /users/:username/inbox
Content-Type: application/activity+json
```
