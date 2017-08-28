Webapp Framework
================

Contents:

.. toctree::
   :maxdepth: 2


Outline
-------


#. Overall project layout - partial design::

     projectRoot
     |
     |-- src
     |  |-- Models
     |  |  |
     |  |  |-- User
     |  |  |   \-- Types
     |  |  |
     |  |  |-- Customer
     |  |  |   \-- Types
     |  |  |
     |  |  |-- Order
     |  |  |   \-- Types
     |  |  |
     |  |  \-- (and so on)
     |  |  
     |  |-- Endpoints
     |  |  |
     |  |  |-- User
     |  |  |   \-- Types
     |  |  |
     |  |  |-- Customer
     |  |  |   \-- Types
     |  |  |
     |  |  |-- Order
     |  |  |   \-- Types
     |  |  |
     |  |  \-- (and so on)
     |  |  |
     |  \-- Foundation
     |
     |-- app
     |  |
     |  \-- Main
     | 
     |
     |-- autogen
     |  |
     |  \-- AutoGenarated
     |     |
     |     |-- Models
     |     |  |-- User
     |     |  |-- Customer
     |     |  |-- Order
     |     |  \-- (and so on)
     |     |
     |     |-- PrimaryKeys
     |     |  |-- UserId
     |     |  |-- CustomerId
     |     |  |-- OrderId
     |     |  \-- (and so on)
     |     |
     |     \-- Classes (used for lenses)
     |        |-- Id
     |        |-- Name
     |        |-- Phone
     |        \-- (and so on)
     |
     \-- scripts

#. Models / Database

   #. Naming conventions - almost final design
   #. Migrations: Creating and editing models - almost final
   #. Strict validations - WIP
   #. Query helpers - partial design
   #. DB transactions & savepoints - partial design

#. Creating JSON APIs - WIP

   #. Basic JSON API - almost final
   #. API-specific validations - WIP
   #. File-uploads - WIP

#. Frontend/UI code

   #. Communicating with JSON APIs - WIP
   #. Validations - WIP
   #. Static assets - WIP

#. Logging

   #. File based logging - almost final
   #. Exception/error notifications - WIP
   #. Performance metrics in production - WIP

#. Sending emails - almost final
#. Job queues - partial design
#. Testing - WIP
#. Deployment - WIP
#. Authentication & authorization - WIP
#. Audit logs - partial design
