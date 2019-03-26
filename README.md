# hydra-posframe

hydra-posframe is a hydra extension which shows hydra hints on posframe.

![hydra-posframe](./screenshot/hydra-posframe.gif)

## Installation

```
;; NOTE: required hydra and posframe
(use-package hydra-posframe
  :load-path "<path-to-the-hydra-posframe>"
  :hook (after-init . hydra-posframe-enable))
```

## Customization

### Variables
- **hydra-posframe-parameters**: override the posframe parameters(e.g. fringe)
- **hydra-posframe-border-width**: change the posframe border width (default: `1`)

### Faces
- **hydra-posframe-face**: customize the posframe fore/background
- **hydra-posframe-border-face**: customize the posframe border color (default: `gray50`)
