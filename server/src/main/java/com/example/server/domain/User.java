package com.example.server.domain;

import javax.persistence.*;

/**
 * Created by scottmcallister on 2018-05-05.
 */
@Entity
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String email;
    private String password;

}
