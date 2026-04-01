# read-later.el
Warning: Just recieved notification from Instapaper that they will be upgrading authentication from oauth 1.0 to 2.0. No date was specified thus I'll keep this update with the latest info. Applications that use the Instapaper API will also need to be linked to a user account after September 30th. I'll be updating the details for authenticating whenever they become available.

Manage your Instapaper account using Emacs! This project was inspired by the use of Instapaper on my Kobo e-reader. This package is for Emacs users that want to easily manage all their Instapaper bookmarks, highlights and folders. It also integrates with other packages like [Elfeed](https://github.com/skeeto/elfeed) to allow users to add articles to Instapaper without ever leaving the Emacs experience. You can read more about the project here: https://devdeveloper.ca/projects/read-later-el/

## Features

- Add URLs to your Instapaper account from anywhere in Emacs
- Integration with elfeed for saving RSS feed entries to Instapaper
- View your bookmarks and tags
- Fully manage your bookmarks
  - Delete bookmarks
  - Mark bookmarks as read or unread
  - Mark bookmarks as archived
  - Filter bookmarks by tag
  - Filter bookmarks by folder
  - More comming soon!
 
## Caveats
Currently bookmarks need to be pulled down each time you start a new Emacs session and aren't persisted like in other packages like Elfeed. I'll be imlementing full sync functionality with SQLite eventually but I want to provide the management facilities early on so that users could start managing their bookmarks while I setup the syncing functionality.

## Prerequisites

- Emacs 27.1 or higher
- An Instapaper account (see setup below)
- Instapaper OAuth Consumer Key and Secret (see instructions below)

## Installation

### Using use-package with straight.el

```elisp
(use-package read-later
  :straight (read-later :type git :host github :repo "barnacleDevelopments/read-later.el"))
```

## Setting Up Instapaper

### 1. Create an Instapaper Account
Go to [https://www.instapaper.com/](https://www.instapaper.com/)

#### a. Request Full API Credentials from Instapaper
To protect the Instapaper API, API keys cannot be included as part of this package to avoid abuse of their API that they so graciously provide for free. For you to gain access to Instapaper's Full API and take advantage of all the read-later.el functionality, you'll need to acquire your own set of key + secret from them. Eventually, I would like to setup an intermediary that can handle managing consumer credentials so that users don't require them to use this package. For the time being, if you would like to take advantage of this package's full functionality you'll need supply your own by contacting Instapaper:

Request your OAuth consumer token here: [https://www.instapaper.com/main/request_oauth_consumer_token](https://www.instapaper.com/main/request_oauth_consumer_token)

**Example request message:**
```
Hi Instapaper team,

I'm using the read-later.el package for Emacs (https://github.com/barnacleDevelopments/read-later.el)
and would like to request OAuth consumer credentials to access the Full API.

This will allow me to use the package's expanded functionality for managing my Instapaper bookmarks
directly from my editor.

Thank you for providing such a great service!

Best regards,
[Your Name]
```

### 2. Configure Authentication in Emacs

read-later uses Emacs' built-in `auth-source` for secure credential storage. You'll need to add your Instapaper credentials to one of these files:

#### Option 1: Using ~/.authinfo (Plain Text)

Add these lines to `~/.authinfo`:

```
machine www.instapaper.com login your-email@example.com password your-password
machine instapaper-oauth login your-consumer-key password your-consumer-secret
```

**Note:** This file stores credentials in plain text. Make sure it has restrictive permissions:

```bash
chmod 600 ~/.authinfo
```

The first line contains your Instapaper account credentials, and the second line contains your OAuth consumer key and secret (obtained by contacting Instapaper).

#### Option 2: Using ~/.authinfo.gpg (Encrypted - Recommended)

For better security, and to avoid your account getting banned if your credentials are used to abuse the API, use an encrypted file:

1. Create or edit `~/.authinfo.gpg`
2. Add the same lines as above:
   ```
   machine www.instapaper.com login your-email@example.com password your-password
   machine instapaper-oauth login your-consumer-key password your-consumer-secret
   ```
3. Save the file (Emacs will prompt you to encrypt it with GPG)

Make sure you have GPG set up and configured in Emacs. The file will be automatically decrypted when accessed.

#### Option 3: Using 1Password

You can store your credentials in [1Password](https://1password.com) using the [`auth-source-1password`](https://github.com/dlobraico/auth-source-1password) package and the [1Password CLI](https://developer.1password.com/docs/cli/).

1. Install the 1Password CLI (`op`) and sign in
2. Install `auth-source-1password`:
   ```elisp
   (use-package auth-source-1password
     :ensure t
     :config
     (auth-source-1password-enable))
   ```
3. Create a 1Password item named `www.instapaper.com` with:
   - A field named `username` containing your Instapaper email
   - A field named `password` containing your Instapaper password
4. Create a second 1Password item named `instapaper-oauth` with:
   - A field named `username` containing your OAuth consumer key
   - A field named `password` containing your OAuth consumer secret
5. Configure read-later to use 1Password:
   ```elisp
   (setq read-later-api-auth-backend '1password)
   ```

If your 1Password item has a different name, override the lookup host:
```elisp
(setq read-later-api-host "My Instapaper Item")
```

> **Note:** When authenticating, you may be prompted for your 1Password master password up to four times on first use of the Full API (once each for the username and password fields of both your account and OAuth consumer credentials). The Simple API prompts twice. Subsequent calls will not prompt again as credentials are cached for the session.
>
> To minimize prompts, choose one of the following:
> - **Desktop app integration** *(recommended)* — in 1Password → Settings → Developer, enable "Integrate with 1Password CLI". The CLI will use the desktop app's unlock state (biometric or system password), so no separate signin is needed.
> - **Shell login** — add `eval $(op signin)` to your `~/.bash_profile` or `~/.zprofile` to authenticate once at login. Sessions last 30 minutes by default.
> - **Extend session timeout** — add `"session_timeout": 86400` to `~/.config/op/config` to keep the session alive for 24 hours.

#### Option 4: Using password-store (Encrypted)

You can securely store your credentials with [`pass`](https://www.passwordstore.org) using the [`password-store.el`](https://git.zx2c4.com/password-store/tree/contrib/emacs/password-store.el) Emacs integration.

1. Create a pass entry for Instapaper: `pass insert www.instapaper.com`
2. Add your username to the pass entry: `pass edit www.instapaper.com`
   ```
   your-password
   user: your-email@example.com
   ```
3. Install and enable `password-store.el` (`M-x package-install RET password-store`)
   ```
   (require 'password-store)
   (auth-source-pass-enable)
   ```

Instapaper should now be able to access your password-store credentials.

### 3. Test Your Configuration

Run these commands in Emacs to verify your credentials work:

**Test your Instapaper account credentials:**
```elisp
M-x read-later-test-auth
```

**Test your OAuth credentials:**
This function gets called every time you make a request using the full-api to give you a fresh access token if needed. 

```elisp
M-x read-later-api-oauth-setup
```

You should see success messages in your minibuffer.

## Usage
Refer to the info docs.

### Integration with elfeed

If you use [elfeed](https://github.com/skeeto/elfeed) for RSS feeds, you can send feed entries directly to Instapaper.

#### Setting up elfeed

First, install and configure elfeed:

```elisp
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("https://example.com/feed.xml"
          "https://another-blog.com/rss")))
```

#### Add elfeed Entry to Instapaper

In the elfeed search buffer, place your cursor on an entry and run:

```elisp
M-x read-later-add-elfeed-entry-at-point
```

## Complete Configuration Example

Here's a complete example configuration:

```elisp
;; Install elfeed
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://sachachua.com/blog/feed/"
          "https://www.reddit.com/r/emacs/.rss")))

;; Install and configure read-later
(use-package read-later
  :straight (read-later :type git :host github :repo "barnacleDevelopments/read-later.el")
  :bind (("C-c i a" . read-later-add-url-at-point)
         ("C-c i u" . read-later-interactively-add-url)
         :map elfeed-search-mode-map
         ("i" . read-later-add-elfeed-entry-at-point)))
```

## Troubleshooting

### Authentication Issues

If you see "✗ Authentication failed":

1. Verify your credentials in `~/.authinfo` or `~/.authinfo.gpg`
2. Make sure you're using your actual Instapaper email and password
3. Run `M-x read-later-test-auth` to test authentication
4. Check that the machine name is exactly `www.instapaper.com`

### auth-source Not Finding Credentials

Make sure auth-source is configured to look in the right places:

```elisp
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
```

### GPG Encryption Issues

If you have trouble with `.authinfo.gpg`:

1. Ensure you have GPG installed: `gpg --version`
2. Configure EPA (EasyPG) in Emacs:
   ```elisp
   (require 'epa-file)
   (epa-file-enable)
   ```

## API Documentation

This package uses both the Instapaper Simple API and Full API (with OAuth). For more information, see:
- Simple API: [https://www.instapaper.com/api/simple](https://www.instapaper.com/api/simple)
- Full API: [https://www.instapaper.com/api/full](https://www.instapaper.com/api/full)

## License

MIT License - see read-later.el for full license text.

## Contributing

Issues and pull requests are welcome at [https://github.com/barnacleDevelopments/read-later.el](https://github.com/barnacleDevelopments/read-later.el)

## Author

Devin Davis
