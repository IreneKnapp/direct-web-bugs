CREATE TABLE players (
    player_id BLOB PRIMARY KEY,
    primary_email TEXT
    REFERENCES emails (email)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    given_name TEXT,
    join_timestamp INTEGER
);


CREATE TABLE emails (
    email TEXT PRIMARY KEY,
    player_id BLOB
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
    creating_proposal_id BLOB,
    number TEXT UNIQUE,
    name TEXT,
    PRIMARY KEY (rule_id, version_id)
);


CREATE TABLE rule_paragraphs (
    rule_id BLOB,
    version_id BLOB,
    paragraph_index INTEGER,
    PRIMARY KEY (rule_id, version_id, paragraph_index),
    FOREIGN KEY (rule_id, version_id)
    REFERENCES rules (rule_id, version_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE rule_sentences (
    rule_id BLOB,
    version_id BLOB,
    paragraph_index INTEGER,
    sentence_index INTEGER,
    text TEXT,
    PRIMARY KEY (rule_id, version_id, paragraph_index, sentence_index),
    FOREIGN KEY (rule_id, version_id, paragraph_index)
    REFERENCES rule_paragraphs (rule_id, version_id, paragraph_index)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE proposals (
    proposal_id BLOB PRIMARY KEY,
    proposer_id BLOB REFERENCES players (player_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    status TEXT,
    creation_timestamp INTEGER,
    voting_start_timestamp INTEGER,
    voting_end_timestamp INTEGER
);


CREATE TABLE proposal_non_normative_text (
    proposal_id BLOB,
    proposal_part_index INTEGER,
    PRIMARY KEY (proposal_id, proposal_part_index)
);


CREATE TABLE proposal_rule_creations (
    proposal_id BLOB,
    proposal_part_index INTEGER,
    number TEXT UNIQUE,
    name TEXT,
    start_paragraph_index INTEGER,
    end_paragraph_index INTEGER,
    PRIMARY KEY (proposal_id, proposal_part_index)
);


CREATE TABLE proposal_rule_deletions (
    proposal_id BLOB,
    proposal_part_index INTEGER,
    rule_id BLOB,
    PRIMARY KEY (proposal_id, proposal_part_index)
);


CREATE TABLE proposal_rule_edits (
    proposal_id BLOB,
    proposal_part_index INTEGER,
    PRIMARY KEY (proposal_id, proposal_part_index),
    FOREIGN KEY (proposal_id) REFERENCES proposals (proposal_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE proposal_rule_edit_paragraphs (
    proposal_id BLOB,
    proposal_part_index INTEGER,
    rule_edit_part_index INTEGER,
    action TEXT,
    rule_paragraph_index INTEGER,
    PRIMARY KEY (proposal_id, proposal_part_index, rule_edit_part_index),
    FOREIGN KEY (proposal_id) REFERENCES proposals (proposal_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE proposal_paragraphs (
    proposal_id BLOB,
    proposal_part_index INTEGER,
    paragraph_index INTEGER,
    PRIMARY KEY (proposal_id, proposal_part_index, paragraph_index),
    FOREIGN KEY (proposal_id) REFERENCES proposals (proposal_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE proposal_sentences (
    proposal_id BLOB,
    proposal_part_index INTEGER,
    paragraph_index INTEGER,
    sentence_index INTEGER,
    text TEXT,
    PRIMARY KEY (proposal_id, proposal_part_index,
                 paragraph_index, sentence_index),
    FOREIGN KEY (proposal_id, proposal_part_index, paragraph_index)
    REFERENCES proposal_paragraphs
               (proposal_id, proposal_part_index, paragraph_index)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE votes (
    proposal_id BLOB REFERENCES proposals (proposal_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    player_id BLOB REFERENCES players (player_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
    vote TEXT,
    cast_timestamp INTEGER,
    PRIMARY KEY (proposal_id, player_id)
);
