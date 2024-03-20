package com.nttdata.product.repository;

import com.nttdata.product.model.Product;
import reactor.core.publisher.Mono;

/**
 * Class: ProductRepository. <br/>
 * <b>Bootcamp NTTDATA</b><br/>
 *
 * @author NTTDATA
 * @version 1.0
 *   <u>Developed by</u>:
 *   <ul>
 *   <li>Developer Carlos</li>
 *   </ul>
 * @since 1.0
 */
public interface ProductRepository {

  Mono<Product> findProductById(String productId);

  Mono<Product> findProduct(String type);

  Mono<Boolean> findExistsProductById(String productId);

  Mono<Boolean> findExistsProduct(String type);

  Mono<Product> saveProduct(Product product);

  Mono<Void> deleteProduct(String productId);
}
