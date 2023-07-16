# hydra-posframe

hydra-posframe is a hydra extension which shows hydra hints on posframe.

![hydra-posframe](./screenshot/hydra-posframe.gif)

## Installation

```
;; NOTE: hydra and posframe are required
(use-package hydra-posframe
  :el-get "Ladicle/hydra-posframe"
  :hook (after-init . hydra-posframe-mode))
```

## Customization

### Variables
- **hydra-posframe-parameters**: override the posframe parameters (e.g. fringe) (default: `nil`)
- **hydra-posframe-border-width**: change the posframe border width (default: `1`)
- **hydra-posframe-poshandler**: posframe position (default: `'posframe-poshandler-frame-center`)
- **hydra-posframe-font**: font for posframe (default: `nil`)

### Faces
- **hydra-posframe-face**: customize the posframe fore/background
- **hydra-posframe-border-face**: customize the posframe border color (default: `gray50`)
