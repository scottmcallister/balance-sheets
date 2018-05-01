package com.example.web.service;

import com.example.support.exception.DuplicateException;
import com.example.support.exception.NotFoundException;
import com.example.web.entity.Author;

/**
 * Author service Implementation refer to {@link AuthorServiceImpl}
 * 
 * @author Wenbo Wang (jackie-1685@163.com)
 */
public interface AuthorService {

	/**
	 * Get a author by its ID
	 * 
	 * @param authorId
	 *            long required
	 * 
	 */
	public Author getAuthor(Long authorId) throws NotFoundException;

	/**
	 * Create a author by its name
	 * 
	 * @param authorName
	 *            String required
	 * 
	 */
	public Author createAuthor(String authorName) throws DuplicateException;

	/**
	 * Delete a author by its ID
	 * 
	 * @param id
	 *            long required
	 * 
	 */
	public void deleteAuthor(Long authorId) throws NotFoundException;

	/**
	 * Assign author a book
	 * 
	 * @param authorId
	 *            long required
	 * @param bookId
	 *            long required
	 * 
	 * 
	 */
	public Author assignBook(Long authorId, Long bookId) throws NotFoundException;
}
