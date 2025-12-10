# read-later.el
Manage your Instapaper account using Emacs!

## Features

- Add URLs to your Instapaper account from anywhere in Emacs
- Integration with elfeed for saving RSS feed entries to Instapaper
- View your bookmarks and tags
- Fully manage your bookmarks (coming soon)

## Full API Status
The package currently supports OAuth 1.0 authentication with the Instapaper Full API. While you can authenticate and obtain access tokens, additional Full API functionality (such as managing folders, highlights, and advanced bookmark operations) is coming soon. Stay tuned for updates!

## Prerequisites

- Emacs 29.1 or higher
- An Instapaper account (see setup below)
- Instapaper OAuth Consumer Key and Secret (see instructions below)

## Installation

### Manual Installation

1. Clone this repository or download `read-later.el`
2. Add to your Emacs load path:

```elisp
(add-to-list 'load-path "/path/to/read-later")
(require 'read-later)
```

### Using straight.el

```elisp
(straight-use-package
 '(read-later :type git :host github :repo "barnacleDevelopments/read-later.el"))
```

### Using use-package with straight.el

```elisp
(use-package read-later
  :straight (read-later :type git :host github :repo "barnacleDevelopments/read-later.el")
  :commands (read-later-add-url-at-point
             read-later-interactively-add-url
             read-later-add-elfeed-entry-at-point))
```

### Spacemacs

Add to your `dotspacemacs-additional-packages`:

```elisp
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-additional-packages
   '((read-later :location (recipe
                             :fetcher github
                             :repo "barnacleDevelopments/read-later.el")))))
```

Then add keybindings in `dotspacemacs/user-config`:

```elisp
(defun dotspacemacs/user-config ()
  (spacemacs/set-leader-keys
    "ai a" 'read-later-add-url-at-point
    "ai u" 'read-later-interactively-add-url)

  ;; For elfeed integration
  (with-eval-after-load 'elfeed
    (define-key elfeed-search-mode-map (kbd "i") 'read-later-add-elfeed-entry-at-point)))
```

### Doom Emacs

Add to your `packages.el`:

```elisp
;; ~/.doom.d/packages.el
(package! read-later
  :recipe (:host github :repo "barnacleDevelopments/read-later.el"))
```

Run `doom sync` after adding the package, then restart Emacs.

## Setting Up Instapaper

### 1. Create an Instapaper Account

1. Go to [https://www.instapaper.com/](https://www.instapaper.com/)
2. Click "Sign Up" in the top right
3. Enter your email address and choose a password
4. Verify your email address

#### a. Request Full API Credentials from Instapaper
To protect the Instapaper API, API keys cannot be included as part of this package to avoid abuse of their API that they so graciously provide for free. For you to gain access to Instapaper's Full API and take advantage of all the read-later.el functionality, you'll need to acquire your own set of keys from them.

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

#### Option 3: Using password-store (Encrypted)

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

Instapapier should now be able to access your password-store credentials.

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

### View Bookmarks

To open the Instapaper bookmarks buffer:

```elisp
M-x read-later
```

This opens a tabulated list of your Instapaper bookmarks where you can:
- Press `g` to refresh the list
- View bookmark titles, reading progress, tags, and descriptions

### Refresh Bookmarks

If you have the bookmarks buffer open, you can refresh it:

```elisp
M-x read-later-update
```

Or simply press `g` in the bookmarks buffer.

### Add URL at Point

Place your cursor on a URL in any buffer and run:

```elisp
M-x read-later-add-url-at-point
```

### Add URL Interactively

To manually enter a URL:

```elisp
M-x read-later-interactively-add-url
```

You'll be prompted to enter the URL in the minibuffer.

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

Suggested keybinding for elfeed-search-mode:

```elisp
(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
              ("i" . read-later-add-elfeed-entry-at-point))
  :config
  (setq elfeed-feeds
        '("https://example.com/feed.xml"
          "https://another-blog.com/rss")))
```

Now you can press `i` on any entry in elfeed to save it to Instapaper.

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
