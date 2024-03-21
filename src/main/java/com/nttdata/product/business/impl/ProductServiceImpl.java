package com.nttdata.product.business.impl;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.builder.ProductBuilder;
import com.nttdata.product.business.ProductService;
import com.nttdata.product.model.Product;
import com.nttdata.product.repository.ProductRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

/**
 * Class: ProductServiceImpl. <br/>
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
@Slf4j
@Service
public class ProductServiceImpl implements ProductService {

  @Autowired
  private ProductRepository productRepository;

  @Override
  public Mono<Product> getProduct(String productType) {
    return productRepository.findProduct(productType)
      .switchIfEmpty(
        Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Products not found - "
          + "type: ".concat(productType))));
  }

  @Override
  public Mono<Product> saveProduct(ProductRequest productRequest) {
    return productRepository.findExistsProduct(productRequest.getType().name())
      .flatMap(exists -> Boolean.FALSE.equals(exists)
        ? productRepository.saveProduct(ProductBuilder.toProductEntity(productRequest))
        : Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST,
          "The product already exists - type: ".concat(productRequest.getType().name()))));
  }

  @Override
  public Mono<Product> updateProduct(ProductRequest productRequest, String productId) {
    return productRepository.findProductById(productId)
      .flatMap(product ->
        productRepository.saveProduct(
          ProductBuilder.toProductEntity(productRequest, product)))
      .switchIfEmpty(
        Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - "
          + "productId: ".concat(productId))));
  }

  @Override
  public Mono<Void> deleteProduct(String productId) {
    return productRepository.findExistsProductById(productId)
      .flatMap(exists -> Boolean.TRUE.equals(exists)
        ? productRepository.deleteProduct(productId)
        : Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - "
          + "productId: ".concat(productId)))
      );
  }
}
