create sequence thread_id_seq start 1;

create table threads (
    id integer primary key not null default nextval('thread_id_seq'),
    title varchar(100),
    created timestamp with time zone
);
