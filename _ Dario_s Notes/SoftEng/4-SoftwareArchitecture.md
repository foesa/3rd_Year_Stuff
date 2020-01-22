# Definition
> Fundamental concepts or properties of a system in its environment embodied in its elements, relationships, and in the principles of its design and evolution

> The software architecture of a program or computing system is a depiction of the system that aids in the understanding of how the system will behave

# Perspectives
- Architecture views are representations of the overall architecture that are meaningul to one or more stakeholders in the system
- The architect chooses and develops a set of views that will enable to arhictecture to be communication and understood by all the stakeholders and enable them to verify that the system will address their concerns

## Design
> All architecture is design but not all design is architecture. Architecture represents that significant design decisions that shape the system.

- Cost of change
    - Security
	- Scalability
	- Availability
	- Privacy

### Security
- Encryption
    - Do transactions need to be encrypted?
	- What's the legal minimum required?
- User identification
    - Cookies
	- Certificates
	- Bio-metrics

### Availability
- How much downtime is acceptable?
- How much data loss is acceptable?
- In a crisis, what is the target time for resuming service?

### Scalability
- How much additional traffic can the system handle?
- How easy is it to add capacity?
- Deployment
    - How many servers will be required?
	- Are these servers geographically separated?
	- Is the infastructure owned by the company? Leased?

### Tradeoffs
- Balancing concerns
    - Each stakeholder prioritises his/her concerns
	- The architect needs to balance all of them
	- Never be a purist; mix-n-match styles to achieve goals

# Attributes
