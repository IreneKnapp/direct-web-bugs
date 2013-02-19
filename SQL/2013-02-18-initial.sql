CREATE TABLE players (
    player_id BLOB PRIMARY KEY,
    primary_email TEXT NOT NULL
    REFERENCES emails (email)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    personal_name TEXT,
    join_timestamp INTEGER
);


CREATE TABLE emails (
    email TEXT PRIMARY KEY,
    player_id BLOB NOT NULL
    REFERENCES players (player_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    confirmation_timestamp INTEGER
);


CREATE TABLE email_confirmation_codes (
    email TEXT PRIMARY KEY
    REFERENCES emails (email)
    ON DELETE CASCADE
    ON UPDATE RESTRICT,
    expiration_timestamp INTEGER,
    code TEXT
);


CREATE TABLE rules (
    rule_id BLOB,
    version_id BLOB,
    number TEXT UNIQUE,
    name TEXT,
    status TEXT
    CHECK (status IN ('active', 'repealed', 'replaced')),
    PRIMARY KEY (rule_id, version_id)
);


CREATE TABLE rule_paragraphs (
    rule_id BLOB,
    version_id BLOB,
    paragraph_index INTEGER
    CHECK (paragraph_index >= 0 AND paragraph_index < paragraph_count),
    paragraph_count INTEGER NOT NULL,
    PRIMARY KEY (rule_id, version_id, paragraph_index),
    FOREIGN KEY (rule_id, version_id)
    REFERENCES rules (rule_id, version_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
    FOREIGN KEY (rule_id, version_id, paragraph_count)
    REFERENCES rules (rule_id, version_id, paragraph_count)
    ON DELETE RESTRICT
    ON UPDATE CASCADE
);


CREATE TABLE rule_sentences (
    rule_id BLOB,
    version_id BLOB,
    paragraph_index INTEGER,
    sentence_index INTEGER
    CHECK (sentence_index >= 0 AND sentence_index < sentence_count),
    sentence_count INTEGER NOT NULL
    CHECK (sentence_count > 0),
    content TEXT,
    PRIMARY KEY (rule_id, version_id, paragraph_index, sentence_index),
    FOREIGN KEY (rule_id, version_id, paragraph_index, sentence_count)
    REFERENCES rule_paragraphs (rule_id, version_id, paragraph_index,
                                sentence_count)
    ON DELETE RESTRICT
    ON UPDATE CASCADE
);


CREATE TABLE proposals (
    proposal_id BLOB PRIMARY KEY,
    proposer_id BLOB NOT NULL
    REFERENCES players (player_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    name TEXT NOT NULL,
    status TEXT CHECK (status IN ('draft', 'open', 'passed', 'failed')),
    creation_timestamp INTEGER,
    voting_start_timestamp INTEGER,
    voting_end_timestamp INTEGER
);


CREATE TABLE proposal_actions (
    proposal_id BLOB
    REFERENCES proposals (proposal_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    action_index INTEGER,
    action_id BLOB NOT NULL
    REFERENCES actions (action_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    PRIMARY KEY (proposal_id, action_index)
);


CREATE TABLE places (
    place_id BLOB PRIMARY KEY,
    place_type TEXT
    CHECK (place_type in ('ruleset', 'rule', 'paragraph', 'sentence')),
    rule_id BLOB,
    paragraph_index INTEGER,
    sentence_index INTEGER,
    FOREIGN KEY (rule_id)
    REFERENCES rules (rule_id)
    ON DELETE SET NULL
    ON UPDATE CASCADE,
    FOREIGN KEY (rule_id, paragraph_index)
    REFERENCES rule_paragraphs (rule_id, paragraph_index)
    ON DELETE SET NULL
    ON UPDATE CASCADE,
    FOREIGN KEY (rule_id, paragraph_index, sentence_index)
    REFERENCES rule_sentences (rule_id, paragraph_index, sentence_index)
    ON DELETE SET NULL
    ON UPDATE CASCADE,
    CHECK (CASE place_type
           WHEN 'ruleset' THEN 1
           WHEN 'rule' THEN rule_id IS NOT NULL
           WHEN 'paragraph' THEN rule_id IS NOT NULL
                                 AND paragraph_index IS NOT NULL
           WHEN 'sentence' THEN rule_id IS NOT NULL
                                AND paragraph_index IS NOT NULL
                                AND sentence_index IS NOT NULL
           END)
);


CREATE TABLE actions (
    action_id BLOB PRIMARY KEY,
    action_type TEXT
    CHECK (action_type in ('description', 'add-rule', 'delete-rule',
                           'renumber-rule', 'rename-rule', 'add-paragraph',
                           'delete-paragraph', 'move-paragraph',
                           'add-sentence', 'delete-sentence', 'move-sentence',
                           'edit-sentence', 'replace-string')),
    from_place_id BLOB,
    from_place_type TEXT,
    to_place_id BLOB,
    to_place_type TEXT,
    rule_number TEXT,
    rule_name TEXT,
    old_string TEXT,
    new_string TEXT,
    paragraph_count INTEGER NOT NULL
    CHECK (paragraph_count >= 0),
    FOREIGN KEY (from_place_id, from_place_type)
    REFERENCES places (place_id, place_type)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    FOREIGN KEY (to_place_id, to_place_type)
    REFERENCES places (place_id, place_type)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    CHECK (CASE action_type
           WHEN 'description'
           THEN from_place_id IS NULL
                AND from_place_type IS NULL
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count > 0
           WHEN 'add-rule'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'ruleset'
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NOT NULL
                AND rule_name IS NOT NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count > 0
           WHEN 'delete-rule'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'rule'
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count = 0
           WHEN 'renumber-rule'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'rule'
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NOT NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count = 0
           WHEN 'rename-rule'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'rule'
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NULL
                AND rule_name IS NOT NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count = 0
           WHEN 'add-paragraph'
           THEN from_place_id IS NULL
                AND from_place_type IS NULL
                AND to_place_id IS NOT NULL
                AND to_place_type = 'paragraph'
                AND rule_number IS NULL
                AND rule_name IS NOT NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count >= 1
           WHEN 'delete-paragraph'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'paragraph'
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count = 0
           WHEN 'move-paragraph'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'paragraph'
                AND to_place_id IS NOT NULL
                AND to_place_type = 'paragraph'
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count = 0
           WHEN 'add-sentence'
           THEN from_place_id IS NULL
                AND from_place_type IS NULL
                AND to_place_id IS NOT NULL
                AND to_place_type = 'sentence'
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NOT NULL
                AND paragraph_count = 0
           WHEN 'delete-sentence'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'sentence'
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count = 0
           WHEN 'move-sentence'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'sentence'
                AND to_place_id IS NOT NULL
                AND to_place_type = 'sentence'
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NULL
                AND paragraph_count = 0
           WHEN 'edit-sentence'
           THEN from_place_id IS NOT NULL
                AND from_place_type = 'sentence'
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NULL
                AND new_string IS NOT NULL
                AND paragraph_count = 0
           WHEN 'replace-string'
           THEN from_place_id IS NOT NULL
                AND from_place_type IS NOT NULL
                AND to_place_id IS NULL
                AND to_place_type IS NULL
                AND rule_number IS NULL
                AND rule_name IS NULL
                AND old_string IS NOT NULL
                AND new_string IS NOT NULL
                AND paragraph_count = 0
           END)
);


CREATE TABLE action_paragraphs (
    action_id BLOB
    REFERENCES actions (action_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    paragraph_index INTEGER
    CHECK (paragraph_index >= 0 AND paragraph_index < paragraph_count),
    paragraph_count INTEGER NOT NULL,
    sentence_count INTEGER NOT NULL
    CHECK (sentence_count >= 0),
    FOREIGN KEY (action_id, paragraph_count)
    REFERENCES actions (action_id, paragraph_index)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    PRIMARY KEY (action_id, paragraph_index)
);


CREATE TABLE action_sentences (
    action_id BLOB,
    paragraph_index INTEGER,
    sentence_index INTEGER,
    sentence_count INTEGER NOT NULL,
    content TEXT,
    PRIMARY KEY (action_id, paragraph_index, sentence_index),
    FOREIGN KEY (action_id, paragraph_index, sentence_count)
    REFERENCES action_paragraphs (action_id, paragraph_index, sentence_count)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE votes (
    proposal_id BLOB NOT NULL
    REFERENCES proposals (proposal_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    player_id BLOB NOT NULL
    REFERENCES players (player_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    vote TEXT,
    cast_timestamp INTEGER,
    PRIMARY KEY (proposal_id, player_id)
);


CREATE TABLE events (
    event_id BLOB PRIMARY KEY,
    event_type TEXT
    CHECK (event_type IN ('start-of-game', 'proposal-passage')),
    timestamp INTEGER
);


CREATE TABLE event_actions (
    event_id BLOB
    REFERENCES events (event_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    action_index INTEGER,
    action_id BLOB NOT NULL
    REFERENCES actions (action_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    PRIMARY KEY (event_id, action_index)
);


CREATE TABLE event_rule_effects (
    event_id BLOB
    REFERENCES events (event_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    rule_id BLOB NOT NULL
    REFERENCES rules (rule_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    old_version_id BLOB,
    new_version_id BLOB,
    FOREIGN KEY (rule_id, old_version_id)
    REFERENCES rules (rule_id, version_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    FOREIGN KEY (rule_id, new_version_id)
    REFERENCES rules (rule_id, version_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE
);

