CREATE TABLE saved_items (
    parent_id BLOB
    REFERENCES folders (folder_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    name TEXT NOT NULL CHECK length(name) > 0,
    folder_id BLOB
    REFERENCES folders (folder_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    saved_request_id BLOB
    REFERENCES saved_requests (saved_request_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    UNIQUE (parent_id, name)
);

CREATE TABLE folders (
    folder_id BLOB PRIMARY KEY,
    expanded INTEGER CHECK expanded IN (0, 1)
);

CREATE TABLE recent_requests (
    recent_request_id BLOB PRIMARY KEY,
    REFERENCES requests
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    index INTEGER CHECK index >= 0 AND index < 10,
    request_id BLOB
    REFERENCES requests (request_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);

CREATE TABLE saved_requests (
    saved_request_id BLOB PRIMARY KEY,
    request_id BLOB
    REFERENCES requests (request_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);

CREATE TABLE requests (
    request_id BLOB PRIMARY KEY,
    secure INTEGER NOT NULL CHECK secure IN (0, 1),
    protocol TEXT NOT NULL CHECK protocol IN ('HTTP/1.0', 'HTTP/1.1'),
    server TEXT NOT NULL CHECK length(server) > 0,
    path TEXT NOT NULL CHECK length(path) > 0 AND path LIKE '/%'
);

CREATE TABLE request_headers (
    request_id BLOB NOT NULL
    REFERENCES requests
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    header TEXT NOT NULL CHECK length(header) > 0,
    value TEXT NOT NULL CHECK length(value) > 0, 
    PRIMARY KEY (request_id, header)
);

CREATE TABLE request_body (
    request_id BLOB NOT NULL
    REFERENCES requests
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    value BLOB NOT NULL
);

CREATE TABLE saved_responses (
    request_id BLOB NOT NULL
    REFERENCES requests
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    saved_response_id BLOB PRIMARY KEY,
    secure INTEGER NOT NULL CHECK secure IN (0, 1),
    protocol TEXT NOT NULL CHECK protocol IN ('HTTP/1.0', 'HTTP/1.1'),
    server TEXT NOT NULL CHECK length(server) > 0,
    path TEXT NOT NULL CHECK length(path) > 0 AND path LIKE '/%',
    timestamp INTEGER NOT NULL,
);

CREATE TABLE saved_response_headers (
    saved_response_id BLOB NOT NULL
    REFERENCES saved_responses (saved_response_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    header TEXT NOT NULL CHECK length(header) > 0,
    value TEXT NOT NULL CHECK length(value) > 0, 
    PRIMARY KEY (saved_response_id, header)
);

CREATE TABLE saved_response_body (
    saved_response_id BLOB NOT NULL
    REFERENCES saved_responses (saved_response_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    value BLOB NOT NULL
);

