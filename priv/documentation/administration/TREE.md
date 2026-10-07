# Proposed administration guide tree

The manifest is the source of collection order. Shared pages keep their existing names.

```text
Site administration
├── People and permissions
│   ├── Creating a user [shared]
│   ├── Assign user groups and check access
│   ├── Test and publish access rules
│   ├── Remove access when someone leaves
│   └── Review accounts and shared responsibilities
├── Site settings and modules
│   ├── Choose the site admin or the status site
│   ├── Enable or disable a site feature
│   ├── Add a language and choose its availability
│   └── Set upload sizes and allowed file types
├── Installation and deployment
│   ├── Set up a local development environment [shared]
│   ├── Prepare an acceptance or production environment
│   ├── Deploy and verify a release
│   ├── Configure service startup and restart behaviour
│   └── Find the status site and your site's admin [shared]
├── Configuration and services
│   ├── Change persistent configuration
│   ├── Configure hostnames and HTTPS
│   ├── Configure outbound email and verify delivery
│   ├── Direct test mail to a controlled recipient
│   ├── Configure and check persistent media storage
│   ├── Check or change a site database connection
│   ├── Find the configuration layer to change [shared]
│   └── Configure media sandboxing and an optional media runner
├── Backups and recovery
│   ├── Define what a complete backup includes
│   ├── Create and check a site backup
│   ├── Rehearse a complete site restore
│   ├── Recover one page without restoring the whole site
│   └── backup: List, create, or restore backups [shared]
├── Updates and maintenance
│   ├── Plan and apply an upgrade
│   ├── Prepare and carry out recovery from a failed release
│   └── Run a planned maintenance window
└── Monitoring and troubleshooting
    ├── Check that a site is working
    ├── Read logs and collect useful evidence
    ├── Diagnose a site that will not start
    ├── Diagnose missing or failed email
    ├── Diagnose failed uploads or missing media
    └── Hand over an incident and record recovery
```
