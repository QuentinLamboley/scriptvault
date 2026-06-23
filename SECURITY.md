# Security and privacy

ScriptVault is designed as a local-first tool. It does not send source files,
metadata, hashes, session information or artifact paths to a remote service.

The vault is a normal folder named `.scriptvault` inside the chosen project.
It can contain complete copies of tracked script content. Do not place the vault
in an untrusted shared location. Back up the entire project, including the vault,
using an encrypted storage solution when the project contains sensitive data.

`sv_restore()` and `sv_restore_project()` create a recovery copy by default.
Nevertheless, restoration can overwrite files; use the status and diff commands
before restoring a complete project.


## Reporting a concern

Do not publish sensitive scripts, vault databases, object archives, credentials, participant information, or data extracts in a public GitHub issue. For a security or privacy concern, contact the repository maintainer privately using the contact route published on the repository page.
