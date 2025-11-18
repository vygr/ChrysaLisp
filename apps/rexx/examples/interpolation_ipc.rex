/* interpolation_ipc.rex - String interpolation with IPC demo */

// Demonstrate how interpolation makes IPC cleaner and more powerful

SAY "=== String Interpolation + IPC Demo ==="
SAY ""

// Traditional REXX would require string concatenation:
//   ADDRESS SYSTEM
//   "logger " || user || " performed " || action
//
// Modern REXX with interpolation is much cleaner:

SAY "1. Logging with Interpolation"
SAY "---"

username = "admin"
action = "system_backup"
target = "/var/backups"
timestamp = "2024-01-18T15:30:00Z"

// Clean IPC command with interpolation
SAY "Would send to logger:"
SAY "  '{username} performed {action} on {target} at {timestamp}'"
SAY ""

// Simulating ADDRESS SYSTEM with interpolation
SAY "2. System Commands with Variables"
SAY "---"

directory = "/home/user/documents"
pattern = "*.txt"
max_depth = "2"

SAY "Would execute:"
SAY "  find {directory} -name '{pattern}' -maxdepth {max_depth}"
SAY ""

// Database queries with interpolation
SAY "3. SQL Queries (if we had ADDRESS SQLITE)"
SAY "---"

table_name = "users"
min_age = "18"
country = "USA"

SAY "Would execute SQL:"
SAY "  SELECT * FROM {table_name}"
SAY "  WHERE age >= {min_age} AND country = '{country}'"
SAY ""

// API calls with interpolation
SAY "4. API Requests"
SAY "---"

api_base = "https://api.example.com"
endpoint = "users"
user_id = "12345"
api_key = "secret-key-here"

SAY "Would send HTTP request:"
SAY "  GET {api_base}/{endpoint}/{user_id}"
SAY "  Authorization: Bearer {api_key}"
SAY ""

// Email/notification with interpolation
SAY "5. Notification Templates"
SAY "---"

recipient = "admin@example.com"
subject = "Backup Complete"
hostname = "server01"
backup_size = "2.5GB"
backup_time = "15:30"

SAY "Would send email:"
SAY "  To: {recipient}"
SAY "  Subject: {subject}"
SAY "  Body:"
SAY "    Backup completed on {hostname}"
SAY "    Size: {backup_size}"
SAY "    Time: {backup_time}"
SAY ""

// Configuration file generation
SAY "6. Config File Generation"
SAY "---"

app_name = "MyApp"
app_port = "8080"
app_host = "localhost"
app_env = "production"

SAY "Would generate config:"
SAY "  [server]"
SAY "  name = {app_name}"
SAY "  host = {app_host}"
SAY "  port = {app_port}"
SAY "  environment = {app_env}"
SAY ""

// Complex interpolation with multiple variables
SAY "7. Complex IPC Scenario"
SAY "---"

// Service mesh communication
service_name = "auth-service"
target_service = "user-service"
method = "POST"
path = "/api/v1/validate"
payload_user = "alice"
payload_token = "xyz123"

SAY "Microservice IPC:"
SAY "  FROM: {service_name}"
SAY "  TO: {target_service}"
SAY "  REQUEST: {method} {path}"
SAY "  PAYLOAD: user={payload_user}, token={payload_token}"
SAY ""

SAY "=== Key Benefits ==="
SAY ""
SAY "String interpolation makes IPC code:"
SAY "  ✓ More readable - clear what's being sent"
SAY "  ✓ Easier to maintain - variables are obvious"
SAY "  ✓ Less error-prone - no manual concatenation"
SAY "  ✓ Template-friendly - great for repeated patterns"
SAY ""
SAY "Perfect for ADDRESS/PORTS communication!"

EXIT 0
