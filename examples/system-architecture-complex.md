# System Architecture – Complex Demo

This file contains several clean but fairly complex Mermaid diagrams to stress test rendering and preview flows.

## Emacs Usage
- `M-x md-mermaid-render-current` (or `C-c m r`) → RET (PNG 1400) → RET (assets dir)
- Open the generated `system-architecture-complex-emacs.md` and `M-x markdown-toggle-inline-images`.
- For browser SVG preview, choose “SVG (browser)” at the preset prompt.
 - Reopen last SVG view quickly: `M-x md-mermaid-preview-last-svg`.

## 1) Service Topology

```mermaid
flowchart LR
  %% Layers
  subgraph C[Clients]
    BROWSER[Browser]
    MOBILE[Mobile]
  end

  subgraph E[Edge]
    CDN[CDN]
    APIGW[API Gateway]
    AUTHZ[Auth Service]
  end

  subgraph S[Core Services]
    USER[User Service]
    ORDER[Order Service]
    PAYMENT[Payment Service]
    INVENTORY[Inventory Service]
    SEARCH[Search Service]
  end

  subgraph D[Data]
    DB[(Primary DB)]
    CACHE[(Cache)]
    ES[(Search Index)]
    BUS{{Event Bus}} 
    Q[[Queue]]
  end

  subgraph O[Observability]
    LOGS[Logs]
    METRICS[Metrics]
    TRACES[Traces]
  end

  %% Edges
  BROWSER --> CDN --> APIGW
  MOBILE  --> APIGW
  APIGW --> AUTHZ
  AUTHZ --> APIGW

  APIGW --> USER
  APIGW --> ORDER
  APIGW --> SEARCH

  ORDER --> PAYMENT
  ORDER --> INVENTORY

  %% Data paths
  USER ----> DB
  ORDER ----> DB
  PAYMENT ----> DB
  INVENTORY ----> DB
  SEARCH ----> ES

  %% Caching
  APIGW <--> CACHE
  SEARCH <--> CACHE

  %% Async integration
  ORDER -- publish --> BUS
  BUS -- fanout --> Q
  Q -- worker --> SEARCH

  %% Telemetry
  APIGW --> LOGS
  S --> METRICS
  APIGW --> TRACES
  S --> TRACES
```

## 2) Order Create Flow

```mermaid
sequenceDiagram
  participant Client
  participant API as API Gateway
  participant OS as Order Service
  participant PS as Payment Service
  participant IS as Inventory Service
  participant EB as Event Bus

  Client->>API: POST /orders
  API->>OS: create order request
  OS->>IS: reserve items
  IS-->>OS: items reserved
  par in-parallel
    OS->>PS: initiate payment
    OS->>EB: publish order created
  and
    PS-->>OS: payment authorized
  end
  OS-->>API: order created response
  API-->>Client: 201 created
  note over OS,EB: eventual consistency via downstream consumers
```

## 3) Order State Model

```mermaid
stateDiagram-v2
  [*] --> NEW
  NEW --> PENDING_PAYMENT : items reserved
  PENDING_PAYMENT --> PAID : payment ok
  PENDING_PAYMENT --> CANCELLED : payment failed
  PAID --> FULFILLING : start picking
  FULFILLING --> SHIPPED : hand to carrier
  SHIPPED --> DELIVERED
  CANCELLED --> [*]
  DELIVERED --> [*]
```

## 4) Domain Model Sketch

```mermaid
classDiagram
  class User {
    +id: UUID
    +email: string
    +createdAt: instant
  }
  class Order {
    +id: UUID
    +userId: UUID
    +total: number
    +status: string
  }
  class Payment {
    +id: UUID
    +orderId: UUID
    +amount: number
    +status: string
  }
  class Item {
    +sku: string
    +qty: int
  }

  User "1" --> "*" Order : places
  Order "1" --> "1" Payment : has
  Order "1" --> "*" Item : contains
```

---

CLI Usage:
- Emacs inline PNG: `bash md-mermaid/scripts/md-mermaid.sh -i md-mermaid/examples/system-architecture-complex.md -png1400 -f`
- Browser SVG: `bash md-mermaid/scripts/md-mermaid.sh -i md-mermaid/examples/system-architecture-complex.md -svg -f`
