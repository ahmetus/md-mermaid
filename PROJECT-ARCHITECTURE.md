# md-mermaid Project Architecture

## Overview

The **md-mermaid** project is an Emacs package that enables rendering of Mermaid diagrams within Markdown files. It provides both batch rendering capabilities (via command-line tools) and live preview functionality (via inline Emacs overlays). The system transforms Markdown files containing Mermaid code fences into visual diagrams while preserving the original content.

### Technology Stack
- **Primary Language**: Emacs Lisp (for package core and live mode)
- **Backend Processing**: Python 3 (for rendering scripts)
- **Diagram Generation**: Node.js with Mermaid CLI (mmdc)
- **Browser Engine**: Chromium/Chrome (via Puppeteer for PNG rendering)
- **Output Formats**: PNG (for Emacs inline display), SVG (for browser viewing)

### Key Components
- **Core Package** (`md-mermaid.el`): Main rendering interface and batch processing
- **Live Extension** (`md-mermaid-live.el`): Real-time overlay preview system
- **Python Renderers**: Batch processor and snippet renderer
- **Shell Wrapper**: Command-line interface with preset configurations
- **Asset Management**: Caching system for generated diagrams
- **CLI Tools Manager** (`md-mermaid-tools.el` + transient menus): Installs/upgrades Mermaid CLI + helper binaries, probes npm managers, and exposes one-click actions from the Emacs UI

### Current State
The project is in a mature state with functional batch rendering and live preview capabilities. The live mode has known performance limitations with large diagrams but works well for typical use cases (1-4 small to medium diagrams).

## Component Architecture

```mermaid
graph TB
    subgraph "Emacs Layer"
        A[md-mermaid.el] --> B[md-mermaid-live.el]
        A --> C[Transient Surface]
        C --> C1[Render Commands]
        C --> C2[CLI Tools Menu]
        C2 --> T[md-mermaid-tools.el]
        B --> D[Overlay Manager]
        D --> E[Job Queue System]
        E --> F[Cache Manager]
    end

    subgraph "Processing Layer"
        G[md-mermaid.sh] --> H[md_mermaid_render.py]
        G --> I[md_mermaid_snippet.py]
        H --> J[friendly_errors.py]
        I --> J
    end

    subgraph "External Dependencies"
        K[mmdc - Mermaid CLI] --> L[Node.js Runtime]
        K --> M[Chromium/Chrome]
        N[Python 3 Runtime] --> H
        N --> I
    end

    subgraph "Output"
        O[Markdown Files]
        P[PNG Images]
        Q[SVG Images]
        R[Assets Directory]
    end

    A --> G
    T --> G
    B --> I
    O --> A
    O --> B
    H --> P
    H --> Q
    I --> P
    I --> Q
    P --> R
    Q --> R

    style A fill:#e1f5fe
    style B fill:#f3e5f5
    style C fill:#fff8e1
    style C2 fill:#fff8e1
    style T fill:#ede7f6
    style G fill:#fff3e0
    style K fill:#e8f5e8
```

**Component Responsibilities:**

- **md-mermaid.el**: Core package providing batch rendering commands, configuration management, and user interface
- **md-mermaid-live.el**: Extension for real-time preview with overlay management, job queuing, and caching
- **Transient Surface & Key Customization**: `md-mermaid.el` embeds a top-level transient menu with live state indicators and customizable keybindings (`md-mermaid--transient-menu-keybinding`, `md-mermaid-keymap-prefix`)
- **md-mermaid-tools.el + CLI Tools Menu**: Provides install/update/check commands, npm preference cycling, PATH fixes, log viewers, and debug toggles that run asynchronous shell processes
- **md-mermaid.sh**: Shell wrapper providing preset configurations and command-line interface
- **md_mermaid_render.py**: Batch processor handling full Markdown file parsing and rendering
- **md_mermaid_snippet.py**: Snippet renderer for individual diagram processing
- **mmdc**: Mermaid CLI tool for actual diagram generation

## Directory Structure

```mermaid
graph LR
    A[md-mermaid/] --> B[md-mermaid.el]
    A --> C[md-mermaid-live.el]
    A --> D[scripts/]
    A --> E[examples/]
    A --> F[assets/]
    A --> G[Documentation]

    D --> H[md-mermaid.sh]
    D --> I[md_mermaid_render.py]
    D --> J[md_mermaid_snippet.py]
    D --> K[gen-outlines.sh]
    D --> L[friendly_errors.py]

    E --> M[mermaid-quick-test.md]
    E --> N[system-architecture-complex.md]
    E --> O[parallel-merge-sort.md]

    F --> P[mermaid/]
    P --> Q[diagram1.png]
    P --> R[diagram2.svg]

    G --> S[README.md]
    G --> T[AGENTS.md]
    G --> U[PROJECT-ARCHITECTURE.md]
    G --> V[CONTRIBUTING.md]

    style B fill:#e3f2fd
    style C fill:#f3e5f5
    style D fill:#fff3e0
    style E fill:#e8f5e8
    style F fill:#fce4ec
    style G fill:#f1f8e9
```

**Directory Functions:**

- **Root Directory**: Core Emacs Lisp packages and configuration files
- **scripts/**: All processing scripts (Python renderers, shell wrapper, utilities)
- **examples/**: Test Markdown files for validation and demonstration
- **assets/mermaid/**: Generated diagram cache (PNG and SVG files)
- **Documentation**: README (user guide), AGENTS (validation rules), PROJECT-ARCHITECTURE (this doc), CONTRIBUTING (workflow)

## Module Dependencies

```mermaid
graph TD
    A[md-mermaid.el] --> B[scripts/md-mermaid.sh]
    A --> C[Python Runtime]
    C --> D[md_mermaid_render.py]
    D --> E[friendly_errors.py]
    B --> D
    D --> F[Mermaid CLI mmdc]
    F --> G[Node.js]
    F --> H[Chromium/Chrome]

    I[md-mermaid-live.el] --> A
    I --> J[scripts/md_mermaid_snippet.py]
    J --> E
    J --> F


    style A fill:#e3f2fd
    style I fill:#f3e5f5
    style D fill:#fff3e0
    style J fill:#fff3e0
    style F fill:#e8f5e8
```

**Dependency Flow:**

1. **Core Loop**: md-mermaid.el → shell script → Python renderer → Mermaid CLI
2. **Live Extension**: md-mermaid-live.el → md-mermaid.el (shared utilities) → snippet renderer
3. **External Dependencies**: All renderers depend on Node.js, Mermaid CLI, and browser engine

## CLI Tools Installation & Diagnostics Flow

```mermaid
flowchart LR
    U[User] --> TM[Transient Menu]
    TM --> CT[CLI Tools Menu]
    CT --> MT[md-mermaid-tools.el orchestrator]
    MT -->|spawn shell/npm| SH[scripts/md-mermaid.sh]
    MT --> LOG[Log / Debug Buffers]
    SH --> PY[md_mermaid_render.py]
    SH --> SNIP[md_mermaid_snippet.py]
    PY --> MMDC[mmdc CLI]
    SNIP --> MMDC
    MT --> PATH[PATH & npm preference]
    LOG --> TM
    MMDC --> Assets[(assets/mermaid/)]
```

**How it behaves**

- The top-level transient menu exposes live-status labels (e.g., “Live Mode [ON/OFF]”) and delegates the CLI submenu to `md-mermaid-tools.el`. Users can also rebind both the transient prefix and the CLI menu key from Emacs without touching init files.
- Each CLI action (install, update, version check, PATH repair, npm preference) uses `md-mermaid-cli--run-async-command` helpers to spawn platform-aware commands, stream output into dedicated log buffers, and surface completion notifications back in the transient UI.
- CLI diagnostics feed directly into the same render pipeline as batch/live commands: installers call `scripts/md-mermaid.sh`, which orchestrates the Python renderers and ultimately the Mermaid CLI (`mmdc` + Chromium/Puppeteer). This keeps the installation lifecycle, render jobs, and troubleshooting flows in a single mental model.
4. **Error Handling**: All Python renderers use friendly_errors.py for error classification

## Data Flow

```mermaid
sequenceDiagram
    participant U as User
    participant E as Emacs Buffer
    participant M as md-mermaid.el
    participant S as md-mermaid.sh
    participant P as Python Renderer
    participant C as Mermaid CLI
    participant A as Assets Directory

    Note over U, A: Batch Rendering Workflow
    U->>E: Edit Markdown with Mermaid fences
    U->>M: M-x md-mermaid-render-current
    M->>S: Execute shell wrapper with preset
    S->>P: Run Python renderer
    P->>P: Parse Markdown for ```mermaid blocks
    P->>P: Generate SHA1 signatures for each block
    loop For each mermaid block
        P->>A: Check if cached image exists
        alt Cache miss
            P->>C: Call mmdc with mermaid content
            C->>A: Generate PNG/SVG file
        end
        P->>A: Store image in assets/mermaid/
    end
    P->>E: Write output Markdown with image links
    E->>U: Display rendered document

    Note over U, A: Live Preview Workflow
    U->>M: M-x md-mermaid-live-mode
    M->>P: Snippet renderer for visible blocks
    P->>C: Individual mermaid block rendering
    C->>A: Create/verify cached images
    P->>M: Return image path
    M->>E: Create overlay with PNG
    E->>U: Show inline preview
```

## State Diagram

```mermaid
stateDiagram-v2
    [*] --> Idle

    state "Live Mode States" as Live {
        Idle --> Scanning: on enable/scroll/edit
        Scanning --> Queuing: find mermaid blocks
        Queuing --> Rendering: create jobs
        Rendering --> Complete: on success
        Rendering --> Error: on failure
        Error --> Queuing: retry mechanism
        Complete --> Idle: on success
    }

    state "Batch Rendering States" as Batch {
        Idle --> Parsing: render command
        Parsing --> Processing: find blocks
        Processing --> Generating: call mmdc
        Generating --> Writing: on success
        Generating --> Failed: on error
        Failed --> Idle: return error
        Writing --> Idle: complete
    }

    state "Overlay States" as Overlay {
        Hidden --> Visible: user toggle
        Visible --> Hidden: user toggle
        Visible --> Updating: content changed
        Updating --> Visible: render complete
        Updating --> Error: render failed
        Error --> Visible: fallback/placeholder
    }

    Complete --> Idle
    Writing --> Idle

    note right of Live
        Live mode manages real-time
        preview with debouncing
        and queue management
    end note

    note right of Batch
        Batch mode processes entire
        files with caching
    end note
```

**State Transitions:**

- **Idle**: Default state, system ready for commands
- **Scanning**: Detection of Mermaid blocks in visible range
- **Queuing**: Job creation and queue management
- **Rendering**: Active diagram generation
- **Complete/Idle**: Successful completion, return to idle
- **Error**: Failure handling with retry or fallback

## Entity Relationships

```mermaid
erDiagram
    MARKDOWN_FILE {
        string path
        string content
        timestamp last_modified
    }

    MERMAID_BLOCK {
        string sha1_signature
        string content
        integer start_line
        integer end_line
    }

    DIAGRAM_ASSET {
        string file_path
        string format_type
        timestamp created_at
        string mermaid_signature
    }

    RENDER_JOB {
        string job_id
        string buffer_name
        integer position
        string status
        timestamp created_at
    }

    CACHE_ENTRY {
        string cache_key
        string asset_path
        timestamp last_access
        integer access_count
    }

    MARKDOWN_FILE ||--o{ MERMAID_BLOCK : contains
    MERMAID_BLOCK ||--|| DIAGRAM_ASSET : generates
    RENDER_JOB ||--o{ MERMAID_BLOCK : processes
    CACHE_ENTRY ||--|| DIAGRAM_ASSET : references

    MARKDOWN_FILE {
        string file_name
    }

    MERMAID_BLOCK {
        string block_type
    }

    DIAGRAM_ASSET {
        string file_size
    }

    RENDER_JOB {
        integer retry_count
    }

    CACHE_ENTRY {
        string cache_type
    }
```

**Entity Relationships:**

- **Markdown Files** contain multiple **Mermaid Blocks**
- Each **Mermaid Block** generates one **Diagram Asset**
- **Render Jobs** process specific **Mermaid Blocks**
- **Cache Entries** reference **Diagram Assets** for performance

## Integration Points

### Emacs Integration
- **Package Load**: Standard Emacs package loading and configuration
- **Key Bindings**: Built-in keybindings with override support
- **Mode Integration**: Works with `markdown-mode`
- **Buffer Management**: Handles both file buffers and transient chat buffers

### External Tool Integration
- **Node.js**: Required runtime for Mermaid CLI
- **Mermaid CLI**: Core diagram generation engine
- **Python 3**: Script execution environment
- **Chromium/Chrome**: Required for PNG rendering via Puppeteer

### File System Integration
- **Asset Directory**: Managed caching under `assets/mermaid/`
- **Project Root Detection**: Sophisticated project boundary detection
- **Output Generation**: Sibling file generation with appropriate naming

## Performance Considerations

### Current Limitations
- **Large Diagrams**: Performance degradation with diagrams exceeding viewport height
- **Multi-diagram Files**: Rendering delays with 6+ diagrams due to sequential processing
- **Memory Usage**: Overlay management impacts Emacs performance in very large buffers

### Optimization Strategies
- **Caching**: SHA1-based asset caching prevents unnecessary re-rendering
- **Throttling**: Debounced refresh reduces processing frequency
- **Queue Management**: Bounded concurrent job processing (currently limited to 1)
- **Visibility Scoping**: Only renders visible content for better performance

## Security Considerations

### External Command Execution
- **Input Validation**: Proper escaping of shell arguments and mermaid content
- **Path Sanitization**: Secure handling of input/output file paths
- **Process Isolation**: Separate processes for rendering tasks

### Data Handling
- **Temporary Files**: Secure temporary file creation and cleanup
- **Content Injection**: Protection against malicious mermaid diagram content
- **Resource Limits**: Configurable limits for buffer size and diagram count

## TODO Comments

<!-- TODO: Add detailed performance metrics section with benchmark results -->
<!-- TODO: Document configuration options and their default values -->
<!-- TODO: Include troubleshooting guide for common issues -->
<!-- TODO: Add developer setup instructions and contribution guidelines -->
<!-- TODO: Document API surface for custom extensions -->
<!-- TODO: Include changelog and version history -->
<!-- TODO: Add security considerations for enterprise deployments -->
<!-- TODO: Document integration with other Emacs packages -->

## Summary

The md-mermaid project represents a well-architected Emacs package that successfully bridges Markdown editing with diagram visualization. Its separation of concerns between batch processing (md-mermaid.el) and live preview (md-mermaid-live.el) provides both reliability and interactivity. The modular Python rendering system, combined with external Mermaid CLI integration, creates a flexible foundation that can handle various use cases from simple documentation to complex technical diagrams.

The current implementation delivers robust core functionality with known performance characteristics and clearly defined architectural boundaries. Future development opportunities exist in performance optimization, enhanced error handling, and expanded integration capabilities.
