# Pseudo TypeScript OpenAI Chat – Architecture and Flow

This sample shows a minimal project layout and request flow for a chat API using OpenAI.

## Emacs Usage
- Open this file, then run `M-x md-mermaid-render-current` (or `C-c m r`).
- Accept defaults (PNG 1400, project `assets/mermaid`) to create `ts-openai-chat-arch-emacs.md`.
- Toggle inline images with `M-x markdown-toggle-inline-images`.
- Choose “SVG (browser)” to open a browser preview of `ts-openai-chat-arch-images.md`.
  - Reopen later (no re-render): `M-x md-mermaid-preview-last-svg`.

## CLI Usage
- Emacs inline PNG:
  ```bash
  bash md-mermaid/scripts/md-mermaid.sh -i md-mermaid/examples/ts-openai-chat-arch.md -png1400 -f
  ```
- Browser SVG:
  ```bash
  bash md-mermaid/scripts/md-mermaid.sh -i md-mermaid/examples/ts-openai-chat-arch.md -svg -f
  ```

### Project Layout Diagran:
```mermaid
graph LR
  A[client/index.ts] --> B[server.ts]
  B --> C[routes/chat.ts]
  C --> D[services/openai.ts]
  D --> E[clients/http.ts]
  B --> F[config/env.ts]
  C --> G[middleware/auth.ts]
```

### Request Flow Diagram
```mermaid
sequenceDiagram
  participant UI as Browser/Client
  participant API as server.ts
  participant CH as routes/chat.ts
  participant SVC as services/openai.ts
  participant HTTP as clients/http.ts
  UI->>API: POST /chat {messages}
  API->>CH: validate request
  CH->>SVC: build request (model,key)
  SVC->>HTTP: POST https://api.openai.com
  HTTP-->>SVC: {assistant message}
  SVC-->>CH: response payload
  CH-->>API: 200 OK (JSON)
  API-->>UI: {assistant message}
```
