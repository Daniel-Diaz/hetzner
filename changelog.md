## 0.2.1.1
* base-4.18 support.

## 0.2.1.0
* Add support for primary IPs.
* Add function to set reverse DNS for a primary IP.
* Add test.

## 0.2.0.0
* Allow to attach servers to networks on creation.
* Modify `streamPages` to cover more cases.
* Add support for DNS operations on zones and records.

## 0.1.2.0
* New function: `getTokenFromEnv`. This function allows the user
  to obtain a token from the `HETZNER_API_TOKEN` environment variable.
* Support for (private) networks.
* New instances for the `Token` type: `IsString`, `Show`, `Eq`, `Ord`.

## 0.1.1.0
* Added support for volumes.
* Fixed parsing of HTTP 204 responses.

## 0.1.0.0
* Initial release.
